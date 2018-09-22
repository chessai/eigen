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

import Prelude hiding (read)
import Control.Monad.Primitive (PrimMonad(..), unsafePrimToPrim)
import Data.Kind (Type)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import qualified Foreign.Concurrent as FC
import GHC.Exts (RealWorld)
import GHC.TypeLits (Nat, KnownNat, type (<=))
import Eigen.Internal
  ( Elem
  , Cast(..)
  , CSparseMatrix
  , CSparseMatrixPtr
  , natToInt
  , Row
  , Col
  )
import qualified Eigen.Internal as Internal

newtype MSparseMatrix :: Nat -> Nat -> Type -> Type -> Type where
  MSparseMatrix :: (ForeignPtr (CSparseMatrix a)) -> MSparseMatrix n m s a

type IOSparseMatrix n m   a = MSparseMatrix n m RealWorld a
type STSparseMatrix n m s a = MSparseMatrix n m s a

new :: forall m n p a. (Elem a, KnownNat n, KnownNat m, PrimMonad p) => p (MSparseMatrix n m (PrimState p) a)
new = unsafePrimToPrim $ alloca $ \pm -> do
  let !c_rs = toC $! natToInt @n
      !c_cs = toC $! natToInt @m
  Internal.call $ Internal.sparse_new c_rs c_cs pm
  m <- peek pm
  fm <- FC.newForeignPtr m $ Internal.call $ Internal.sparse_free m
  pure $! MSparseMatrix fm

rows :: forall n m s a. (Elem a, KnownNat n, KnownNat m) => MSparseMatrix n m s a -> Int
rows _ = natToInt @n

cols :: forall n m s a. (Elem a, KnownNat n, KnownNat m) => MSparseMatrix n m s a -> Int
cols _ = natToInt @m

innerSize :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p Int
innerSize = _prop Internal.sparse_innerSize (pure . fromC)

outerSize :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p Int
outerSize = _prop Internal.sparse_outerSize (pure . fromC)

compressed :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p Bool
compressed = _prop Internal.sparse_isCompressed (pure . (== 1))

compress :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p ()
compress = _inplace Internal.sparse_compressInplace

uncompress :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p ()
uncompress = _inplace Internal.sparse_uncompressInplace

read :: forall n m r c p a. (Elem a, PrimMonad p, KnownNat n, KnownNat m, KnownNat r, KnownNat c, r <= n, c <= m)
  => Row r
  -> Col c
  -> MSparseMatrix n m (PrimState p) a
  -> p a
read _ _ (MSparseMatrix fm) =
  let !c_r = toC $! natToInt @r
      !c_c = toC $! natToInt @c
  in unsafePrimToPrim $ withForeignPtr fm $ \m -> alloca $ \px -> do
       Internal.call $ Internal.sparse_coeff m c_r c_c px
       fromC <$> peek px

write :: forall n m r c p a. (Elem a, PrimMonad p, KnownNat n, KnownNat m, KnownNat r, KnownNat c, r <= n, c <= m)
  => MSparseMatrix n m (PrimState p) a -> Row r -> Col c -> a -> p ()
write (MSparseMatrix fm) _ _ x =
  let !c_r = toC $! natToInt @r
      !c_c = toC $! natToInt @c
  in unsafePrimToPrim $ withForeignPtr fm $ \m -> alloca $ \px -> do
       Internal.call $ Internal.sparse_coeffRef m c_r c_c px
       peek px >>= (`poke` toC x)

setIdentity :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p ()
setIdentity = _inplace Internal.sparse_setIdentity

setZero :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p ()
setZero = _inplace Internal.sparse_setZero

reserve :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> Int -> p ()
reserve m s = _inplace (\p -> Internal.sparse_reserve p (toC s)) m

nonZeros :: (Elem a, PrimMonad p) => MSparseMatrix n m (PrimState p) a -> p Int
nonZeros = _prop Internal.sparse_nonZeros (pure . fromC)

_inplace :: (Elem a, PrimMonad p) => (Ptr (CSparseMatrix a) -> IO CString) -> MSparseMatrix n m (PrimState p) a -> p ()
_inplace f (MSparseMatrix fm) = unsafePrimToPrim $ withForeignPtr fm $ \m -> Internal.call $ f m

_prop :: (Storable b, PrimMonad p) => (CSparseMatrixPtr a -> Ptr b -> IO CString) -> (b -> IO c) -> MSparseMatrix n m (PrimState p) a -> p c
_prop f g (MSparseMatrix fp) = unsafePrimToPrim $ 
  withForeignPtr fp $ \p ->
    alloca $ \pq -> do
      Internal.call (f p pq)
      peek pq >>= g

