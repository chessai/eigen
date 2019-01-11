{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Eigen.SparseMatrix.Mutable
  ( 
    -- * Mutable SparseMatrix 
    MSparseMatrix(..)
  , IOSparseMatrix
  , STSparseMatrix
  , new
  , reserve

    -- * SparseMatrix properties
  , rows
  , cols
  , innerSize
  , outerSize
  , nonZeros

    -- * SparseMatrix compression
  , compressed
  , compress
  , uncompress

    -- * Accessing SparseMatrix data
  , read
  , write
  , setZero
  , setIdentity
  ) where

import Control.Monad.Primitive (PrimMonad(..), unsafePrimToPrim)
import Data.Kind (Type)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Exts (RealWorld)
import GHC.TypeLits (Nat, KnownNat)
import Prelude hiding (read)
import Refined
import qualified Foreign.Concurrent as FC

import Eigen.Internal
  ( Elem
  , Cast(..)
  , CSparseMatrix
  , CSparseMatrixPtr
  , natToInt
  )
import qualified Eigen.Internal as Internal

-- | Mutable sparse matrix. See 'Eigen.SparseMatrix.SparseMatrix' for
--   details about matrix layout.
newtype MSparseMatrix :: Nat -> Nat -> Type -> Type -> Type where
  MSparseMatrix :: (ForeignPtr (CSparseMatrix a)) -> MSparseMatrix n m s a

-- | A sparse matrix where the state token is specialised to 'ReadWorld'.
type IOSparseMatrix n m   a = MSparseMatrix n m RealWorld a
-- | This type does not differ from 'MSparseMatrix', but might be desirable
--   for readability.
type STSparseMatrix n m s a = MSparseMatrix n m s a

-- | Create a new sparse matrix with the given size @rows x cols@.
new :: forall m n p a. (Elem a, KnownNat n, KnownNat m, PrimMonad p) => p (MSparseMatrix n m (PrimState p) a)
{-# INLINE new #-}
new = unsafePrimToPrim $ alloca $ \pm -> do
  let !c_rs = toC $! natToInt @n
      !c_cs = toC $! natToInt @m
  Internal.call $ Internal.sparse_new c_rs c_cs pm
  m <- peek pm
  fm <- FC.newForeignPtr m $ Internal.call $ Internal.sparse_free m
  pure $! MSparseMatrix fm

-- | Returns the number of rows of the matrix.
rows :: forall n m s a. (Elem a, KnownNat n, KnownNat m) => MSparseMatrix n m s a -> Int
{-# INLINE rows #-}
rows _ = natToInt @n

-- | Returns the number of columns of the matrix.
cols :: forall n m s a. (Elem a, KnownNat n, KnownNat m) => MSparseMatrix n m s a -> Int
{-# INLINE cols #-}
cols _ = natToInt @m

-- | Returns the number of rows of the matrix.
rows' :: forall n m s a. (Elem a, KnownNat n, KnownNat m) => MSparseMatrix n m s a -> Refined (EqualTo n) Int
{-# INLINE rows' #-}
rows' = _unsafeRefine . rows

-- | Returns the number of columns of the matrix.
cols' :: forall n m s a. (Elem a, KnownNat n, KnownNat m) => MSparseMatrix n m s a -> Refined (EqualTo m) Int
{-# INLINE cols' #-}
cols' = _unsafeRefine . cols

-- | Returns the number of rows (resp. columns) of the matrix if the storage order is column majour (resp. row majour)
innerSize :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p Int
{-# INLINE innerSize #-}
innerSize = _prop Internal.sparse_innerSize (pure . fromC)

-- | Returns the number of columns (resp. rows) of the matrix if the storage order is column majour (resp. row majour)
outerSize :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p Int
{-# INLINE outterSize #-}
outerSize = _prop Internal.sparse_outerSize (pure . fromC)

-- | Returns whether or not the matrix is in compressed form.
compressed :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p Bool
{-# INLINE compressed #-}
compressed = _prop Internal.sparse_isCompressed (pure . (== 1))

-- | Turns the matrix into compressed format.
compress :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p ()
{-# INLINE compress #-}
compress = _inplace Internal.sparse_compressInplace

-- | Decompresses the matrix.
uncompress :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p ()
{-# INLINE uncompress #-}
uncompress = _inplace Internal.sparse_uncompressInplace

-- | Read the value of the matrix at position i,j. This function returns @Scalar(0)@ if the element is an explicit 0.
read :: forall n m p a. (Elem a, PrimMonad p, KnownNat n, KnownNat m)
  => Refined (FromTo 0 n) Int
  -> Refined (FromTo 0 m) Int
  -> MSparseMatrix n m (PrimState p) a
  -> p a
{-# INLINE read #-}
read r c (MSparseMatrix fm) =
  let !c_r = toC $! unrefine r
      !c_c = toC $! unrefine c
  in unsafePrimToPrim $ withForeignPtr fm $ \m -> alloca $ \px -> do
       Internal.call $ Internal.sparse_coeff m c_r c_c px
       fromC <$> peek px

{- | Writes the value of the matrix at position @i@, @j@.
     This function turns the matrix into a non compressed form if that was not the case.

     This is a @O(log(nnz_j))@ operation (binary search) plus the cost of element insertion if the element does not already exist.

     Cost of element insertion is sorted insertion in O(1) if the elements of each inner vector are inserted in increasing inner index order, and in @O(nnz_j)@ for a random insertion.
-}
write :: forall n m p a. (Elem a, PrimMonad p, KnownNat n, KnownNat m)
  => MSparseMatrix n m (PrimState p) a -> Refined (FromTo 0 n) Int -> Refined (FromTo 0 m) Int -> a -> p ()
{-# INLINE write #-}
write (MSparseMatrix fm) r c x =
  let !c_r = toC $! unrefine r
      !c_c = toC $! unrefine c
  in unsafePrimToPrim $ withForeignPtr fm $ \m -> alloca $ \px -> do
       Internal.call $ Internal.sparse_coeffRef m c_r c_c px
       peek px >>= (`poke` toC x)

-- | Sets the matrix to the identity matrix.
setIdentity :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p ()
{-# INLINE setIdentity #-}
setIdentity = _inplace Internal.sparse_setIdentity

-- | Remove all non zeros, but keep allocated memory.
setZero :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p ()
{-# INLINE setZero #-}
setZero = _inplace Internal.sparse_setZero

-- | Preallocates for non zeros. The matrix must be in compressed mode.
reserve :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> Int -> p ()
{-# INLINE reserve #-}
reserve m s = _inplace (\p -> Internal.sparse_reserve p (toC s)) m

-- | Returns the number of nonzero coefficients.
nonZeros :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p Int
{-# INLINE nonZeros #-}
nonZeros = _prop Internal.sparse_nonZeros (pure . fromC)

_inplace :: (Elem a, PrimMonad p) => (Ptr (CSparseMatrix a) -> IO CString) -> MSparseMatrix n m (PrimState p) a -> p ()
{-# INLINEABLE _inplace #-}
_inplace f (MSparseMatrix fm) = unsafePrimToPrim $ withForeignPtr fm $ \m -> Internal.call $ f m

_prop :: (Storable b, PrimMonad p) => (CSparseMatrixPtr a -> Ptr b -> IO CString) -> (b -> IO c) -> MSparseMatrix n m (PrimState p) a -> p c
{-# INLINEABLE _prop #-}
_prop f g (MSparseMatrix fp) = unsafePrimToPrim $ 
  withForeignPtr fp $ \p ->
    alloca $ \pq -> do
      Internal.call (f p pq)
      peek pq >>= g
