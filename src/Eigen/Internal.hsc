--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-} 
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}

--------------------------------------------------------------------------------

-- | Internal module to Eigen.
--   Here we define all foreign function calls,
--   and some typeclasses integral to the public and private interfaces
--   of the library.
module Eigen.Internal where --   FIXME: Explicit export list

--------------------------------------------------------------------------------

import           Control.Monad            (when)
import           Data.Binary              (Binary(put,get))
import           Data.Binary.Get          (getByteString, getWord32be)
import           Data.Binary.Put          (putByteString, putWord32be)
import           Data.Bits                (xor)
import           Data.Complex             (Complex((:+)))
import           Data.Kind                (Type)
import           Data.Proxy               (Proxy(Proxy))
import           Foreign.C.String         (CString, peekCString)
import           Foreign.C.Types          (CInt(CInt), CFloat(CFloat), CDouble(CDouble), CChar)
import           Foreign.ForeignPtr       (ForeignPtr, castForeignPtr, withForeignPtr)
import           Foreign.Ptr              (Ptr, castPtr, nullPtr, plusPtr)
import           Foreign.Storable         (Storable(sizeOf, alignment, poke, peek, peekByteOff, peekElemOff, pokeByteOff, pokeElemOff))
import           GHC.TypeLits             (natVal, KnownNat, Nat)
import           System.IO.Unsafe         (unsafeDupablePerformIO)
import qualified Data.Vector.Storable     as VS
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI

--------------------------------------------------------------------------------

-- | Like 'Proxy', but specialised to 'Nat'.
data Row (r :: Nat) = Row
-- | Like 'Proxy', but specialised to 'Nat'.
data Col (c :: Nat) = Col

-- | Used internally. Given a 'KnownNat' constraint, turn the type-level 'Nat' into an 'Int'.
natToInt :: forall n. KnownNat n => Int
{-# INLINE natToInt #-}
natToInt = fromIntegral (natVal @n Proxy)

--------------------------------------------------------------------------------

-- | Cast to and from a C-FFI type
--   'Cast' is a closed typeclass with an associated injective type family.
--   It is closed in the sense that we provide only four types
--   with instances for it; and intend for eigen to only be used
--   with those four types. The injectivity of the type family is
--   then useful for avoiding MPTCs. 'Cast' has two functions; 'toC'
--   and 'fromC', where 'toC' goes from a Haskell type to its associated
--   C type for internal use, with the C FFI, and 'fromC' goes from the
--   associated C type to the Haskell type.
class Cast (a :: Type) where
  type family C a = (result :: Type) | result -> a
  toC   :: a -> C a
  fromC :: C a -> a

instance Cast Int where
  type C Int = CInt
  toC = CInt . fromIntegral
  {-# INLINE toC #-}
  fromC (CInt x) = fromIntegral x
  {-# INLINE fromC #-}

instance Cast Float where
  type C Float = CFloat
  toC = CFloat
  {-# INLINE toC #-}
  fromC (CFloat x) = x
  {-# INLINE fromC #-}

instance Cast Double where
  type C Double = CDouble
  toC = CDouble
  {-# INLINE toC #-}
  fromC (CDouble x) = x
  {-# INLINE fromC #-}

instance Cast a => Cast (Complex a) where
  type C (Complex a) = CComplex (C a)
  toC (a :+ b) = CComplex (toC a) (toC b)
  {-# INLINE toC #-}
  fromC (CComplex a b) = (fromC a) :+ (fromC b)
  {-# INLINE fromC #-}

-- | WARNING! 'toC' is lossy for any Int greater than (maxBound :: Int32)!
instance Cast a => Cast (Int, Int, a) where
  type C (Int, Int, a) = CTriplet a
  {-# INLINE toC #-}
  toC (x, y, z) = CTriplet (toC x) (toC y) (toC z)
  {-# INLINE fromC #-}
  fromC (CTriplet x y z) = (fromC x, fromC y, fromC z)

--------------------------------------------------------------------------------

-- | Complex number for FFI with the same memory layout as std::complex\<T\>
data CComplex a = CComplex !a !a deriving (Show)

instance Storable a => Storable (CComplex a) where
    sizeOf _ = sizeOf (undefined :: a) * 2
    alignment _ = alignment (undefined :: a)
    poke p (CComplex x y) = do
        pokeElemOff (castPtr p) 0 x
        pokeElemOff (castPtr p) 1 y
    peek p = CComplex
        <$> peekElemOff (castPtr p) 0
        <*> peekElemOff (castPtr p) 1

--------------------------------------------------------------------------------

-- | FIXME: Doc
data CTriplet a where
  CTriplet :: Cast a => !CInt -> !CInt -> !(C a) -> CTriplet a

deriving instance (Show a, Show (C a)) => Show (CTriplet a)

instance (Storable a, Elem a) => Storable (CTriplet a) where
    sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: CInt) * 2
    alignment _ = alignment (undefined :: CInt)
    poke p (CTriplet row col val) = do
        pokeElemOff (castPtr p) 0 row
        pokeElemOff (castPtr p) 1 col
        pokeByteOff p (sizeOf (undefined :: CInt) * 2) val
    peek p = CTriplet
        <$> peekElemOff (castPtr p) 0
        <*> peekElemOff (castPtr p) 1
        <*> peekByteOff p (sizeOf (undefined :: CInt) * 2)

--------------------------------------------------------------------------------

-- | `Elem` is a closed typeclass that encompasses the properties
--   eigen expects its values to possess, and simplifies the external
--   API quite a bit.
class (Num a, Cast a, Storable a, Storable (C a), Code (C a)) => Elem a

instance Elem Float
instance Elem Double
instance Elem (Complex Float)
instance Elem (Complex Double)

--------------------------------------------------------------------------------

-- | Encode a C Type as a CInt
--
--   Hack used in FFI wrapper functions when constructing FFI calls
class Code a where; code :: a -> CInt
instance Code CFloat             where; code _ = 0
instance Code CDouble            where; code _ = 1
instance Code (CComplex CFloat)  where; code _ = 2
instance Code (CComplex CDouble) where; code _ = 3

-- | Hack used in constructing FFI calls.
newtype MagicCode = MagicCode CInt deriving Eq

instance Binary MagicCode where
    put (MagicCode _code) = putWord32be $ fromIntegral _code
    get = MagicCode . fromIntegral <$> getWord32be

-- | Hack used in constructing FFI calls.
magicCode :: Code a => a -> MagicCode
magicCode x = MagicCode (code x `xor` 0x45696730)

--------------------------------------------------------------------------------

-- | Machine size of a 'CInt'.
intSize :: Int
intSize = sizeOf (undefined :: CInt)

-- | FIXME: Doc
encodeInt :: CInt -> BS.ByteString
encodeInt x = BSI.unsafeCreate (sizeOf x) $ (`poke` x) . castPtr

-- | FIXME: Doc
decodeInt :: BS.ByteString -> CInt
decodeInt (BSI.PS fp fo fs)
    | fs == sizeOf x = x
    | otherwise = error "decodeInt: wrong buffer size"
    where x = performIO $ withForeignPtr fp $ peek . (`plusPtr` fo)

--------------------------------------------------------------------------------

-- | 'Binary' instance for 'Data.Vector.Storable.Mutable.Vector'
instance Storable a => Binary (VS.Vector a) where
    put vs = put (BS.length bs) >> putByteString bs where
        (fp,fs) = VS.unsafeToForeignPtr0 vs
        es = sizeOf (VS.head vs)
        bs = BSI.fromForeignPtr (castForeignPtr fp) 0 (fs * es)
        
    get = get >>= getByteString >>= \bs -> let
        (fp,fo,fs) = BSI.toForeignPtr bs
        es = sizeOf (VS.head vs)
        -- `plusForeignPtr` is used qualified here to just remind a reader
        -- that it is defined internally within eigen
        vs = VS.unsafeFromForeignPtr0 (Eigen.Internal.plusForeignPtr fp fo) (fs `div` es)
        in return vs

--------------------------------------------------------------------------------

-- | FIXME: Doc
data CSparseMatrix a
-- | FIXME: Doc
type CSparseMatrixPtr a = Ptr (CSparseMatrix a)

-- | FIXME: Doc
data CSolver a
-- | FIXME: Doc
type CSolverPtr a = Ptr (CSolver a)

-- {-# INLINE unholyPerformIO #-}
-- unholyPerformIO :: IO a -> a
-- unholyPerformIO (IO m) = case m realWorld# of (# _, r #) -> r

-- | FIXME: replace with unholyPerformIO (?)
performIO :: IO a -> a
performIO = unsafeDupablePerformIO

-- | FIXME: Doc
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr fp fo = castForeignPtr fp1 where
    vs :: VS.Vector CChar
    vs = VS.unsafeFromForeignPtr (castForeignPtr fp) fo 0
    (fp1, _) = VS.unsafeToForeignPtr0 vs

foreign import ccall "eigen-proxy.h free" c_freeString :: CString -> IO ()

call :: IO CString -> IO ()
call func = func >>= \c_str -> when (c_str /= nullPtr) $
    peekCString c_str >>= \str -> c_freeString c_str >> fail str

foreign import ccall "eigen-proxy.h free" free :: Ptr a -> IO ()

foreign import ccall "eigen-proxy.h eigen_setNbThreads" c_setNbThreads :: CInt -> IO ()
foreign import ccall "eigen-proxy.h eigen_getNbThreads" c_getNbThreads :: IO CInt

--------------------------------------------------------------------------------

#let api1 name, args = "foreign import ccall \"eigen_%s\" c_%s :: CInt -> %s\n%s :: forall a . Code (C a) => %s\n%s = c_%s (code (undefined :: (C a)))", #name, #name, args, #name, args, #name, #name

#api1 random,        "Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 identity,      "Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 add,           "Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 sub,           "Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 mul,           "Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 diagonal,      "Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 transpose,     "Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 inverse,       "Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 adjoint,       "Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 conjugate,     "Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 normalize,     "Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 sum,           "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 prod,          "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 mean,          "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 norm,          "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 trace,         "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 squaredNorm,   "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 blueNorm,      "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 hypotNorm,     "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 determinant,   "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 rank,          "CInt -> Ptr CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 image,         "CInt -> Ptr (Ptr (C a)) -> Ptr CInt -> Ptr CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 kernel,        "CInt -> Ptr (Ptr (C a)) -> Ptr CInt -> Ptr CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 solve,         "CInt -> Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api1 relativeError, "Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString"

--------------------------------------------------------------------------------

#let api2 name, args = "foreign import ccall \"eigen_%s\" c_%s :: CInt -> %s\n%s :: forall a . Code (C a) => %s\n%s = c_%s (code (undefined :: (C a)))", #name, #name, args, #name, args, #name, #name

#api2 sparse_new,           "CInt -> CInt -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_clone,         "CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_fromList,      "CInt -> CInt -> Ptr (CTriplet a) -> CInt -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_toList,        "CSparseMatrixPtr a -> Ptr (CTriplet a) -> CInt -> IO CString"
#api2 sparse_free,          "CSparseMatrixPtr a -> IO CString"
#api2 sparse_makeCompressed,"CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_uncompress,    "CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_isCompressed,  "CSparseMatrixPtr a -> Ptr CInt -> IO CString"
#api2 sparse_transpose,     "CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_adjoint,       "CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_pruned,        "CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_prunedRef,     "CSparseMatrixPtr a -> Ptr (C a) -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_scale,         "CSparseMatrixPtr a -> Ptr (C a) -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_nonZeros,      "CSparseMatrixPtr a -> Ptr CInt -> IO CString"
#api2 sparse_innerSize,     "CSparseMatrixPtr a -> Ptr CInt -> IO CString"
#api2 sparse_outerSize,     "CSparseMatrixPtr a -> Ptr CInt -> IO CString"
#api2 sparse_coeff,         "CSparseMatrixPtr a -> CInt -> CInt -> Ptr (C a) -> IO CString"
#api2 sparse_coeffRef,      "CSparseMatrixPtr a -> CInt -> CInt -> Ptr (Ptr (C a)) -> IO CString"
#api2 sparse_cols,          "CSparseMatrixPtr a -> Ptr CInt -> IO CString"
#api2 sparse_rows,          "CSparseMatrixPtr a -> Ptr CInt -> IO CString"
#api2 sparse_norm,          "CSparseMatrixPtr a -> Ptr (C a) -> IO CString"
#api2 sparse_squaredNorm,   "CSparseMatrixPtr a -> Ptr (C a) -> IO CString"
#api2 sparse_blueNorm,      "CSparseMatrixPtr a -> Ptr (C a) -> IO CString"
#api2 sparse_add,           "CSparseMatrixPtr a -> CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_sub,           "CSparseMatrixPtr a -> CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_mul,           "CSparseMatrixPtr a -> CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_block,         "CSparseMatrixPtr a -> CInt -> CInt -> CInt -> CInt -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_fromMatrix,    "Ptr (C a) -> CInt -> CInt -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api2 sparse_toMatrix,      "CSparseMatrixPtr a -> Ptr (C a) -> CInt -> CInt -> IO CString"
#api2 sparse_values,        "CSparseMatrixPtr a -> Ptr CInt -> Ptr (Ptr (C a)) -> IO CString"
#api2 sparse_outerStarts,   "CSparseMatrixPtr a -> Ptr CInt -> Ptr (Ptr CInt) -> IO CString"
#api2 sparse_innerIndices,  "CSparseMatrixPtr a -> Ptr CInt -> Ptr (Ptr CInt) -> IO CString"
#api2 sparse_innerNNZs,     "CSparseMatrixPtr a -> Ptr CInt -> Ptr (Ptr CInt) -> IO CString"
#api2 sparse_setZero,       "CSparseMatrixPtr a -> IO CString"
#api2 sparse_setIdentity,   "CSparseMatrixPtr a -> IO CString"
#api2 sparse_reserve,       "CSparseMatrixPtr a -> CInt -> IO CString"
#api2 sparse_resize,        "CSparseMatrixPtr a -> CInt -> CInt -> IO CString"

#api2 sparse_conservativeResize,    "CSparseMatrixPtr a -> CInt -> CInt -> IO CString"
#api2 sparse_compressInplace,       "CSparseMatrixPtr a -> IO CString"
#api2 sparse_uncompressInplace,     "CSparseMatrixPtr a -> IO CString"

--------------------------------------------------------------------------------

#let api3 name, args = "foreign import ccall \"eigen_%s\" c_%s :: CInt -> CInt -> %s\n%s :: forall s a . (Code s, Code (C a)) => s -> %s\n%s s = c_%s (code (undefined :: (C a))) (code s)", #name, #name, args, #name, args, #name, #name

#api3 sparse_la_newSolver,          "Ptr (CSolverPtr a) -> IO CString"
#api3 sparse_la_freeSolver,         "CSolverPtr a -> IO CString"
#api3 sparse_la_factorize,          "CSolverPtr a -> CSparseMatrixPtr a -> IO CString"
#api3 sparse_la_analyzePattern,     "CSolverPtr a -> CSparseMatrixPtr a -> IO CString"
#api3 sparse_la_compute,            "CSolverPtr a -> CSparseMatrixPtr a -> IO CString"
#api3 sparse_la_tolerance,          "CSolverPtr a -> Ptr CDouble -> IO CString"
#api3 sparse_la_setTolerance,       "CSolverPtr a -> CDouble -> IO CString"
#api3 sparse_la_maxIterations,      "CSolverPtr a -> Ptr CInt -> IO CString"
#api3 sparse_la_setMaxIterations,   "CSolverPtr a -> CInt -> IO CString"
#api3 sparse_la_info,               "CSolverPtr a -> Ptr CInt -> IO CString"
#api3 sparse_la_error,              "CSolverPtr a -> Ptr CDouble -> IO CString"
#api3 sparse_la_iterations,         "CSolverPtr a -> Ptr CInt -> IO CString"
#api3 sparse_la_solve,              "CSolverPtr a -> CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
-- #api3 sparse_la_solveWithGuess,     "CSolverPtr a -> CSparseMatrixPtr a -> CSparseMatrixPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api3 sparse_la_matrixQ,            "CSolverPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api3 sparse_la_matrixR,            "CSolverPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api3 sparse_la_setPivotThreshold,  "CSolverPtr a -> CDouble -> IO CString"
#api3 sparse_la_rank,               "CSolverPtr a -> Ptr CInt -> IO CString"
#api3 sparse_la_matrixL,            "CSolverPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api3 sparse_la_matrixU,            "CSolverPtr a -> Ptr (CSparseMatrixPtr a) -> IO CString"
#api3 sparse_la_setSymmetric,       "CSolverPtr a -> CInt -> IO CString"
#api3 sparse_la_determinant,        "CSolverPtr a -> Ptr (C a) -> IO CString"
#api3 sparse_la_logAbsDeterminant,  "CSolverPtr a -> Ptr (C a) -> IO CString"
#api3 sparse_la_absDeterminant,     "CSolverPtr a -> Ptr (C a) -> IO CString"
#api3 sparse_la_signDeterminant,    "CSolverPtr a -> Ptr (C a) -> IO CString"

--------------------------------------------------------------------------------
