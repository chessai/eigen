{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Eigen.Matrix.Mutable
  ( -- * Types
    MMatrix(..)
  , MMatrixXf
  , MMatrixXd
  , MMatrixXcf
  , MMatrixXcd
  , IOMatrix
  , STMatrix

    -- * Construction
  , new
  , replicate

    -- * Indexing
  , read
  , write

    -- * Modification
  , set
  , copy

    -- * Modification with Pointers
  , unsafeWith
  
    -- * Conversion to Vectors
  , vals
  , fromVector
  ) where

import Data.Eigen.Internal
  ( Elem
  , C(..)
  , natToInt
  , Row(..)
  , Col(..)
  )

import Prelude hiding (replicate, read)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Complex (Complex)
import Data.Kind (Type)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import GHC.Exts (RealWorld)
import GHC.TypeLits (Nat, type (*), type (<=), KnownNat)
import qualified Data.Vector.Storable.Mutable as VSM

data MMatrix :: Nat -> Nat -> Type -> Type -> Type where
  MMatrix :: Elem a => Vec (n * m) s a -> MMatrix n m s a

data Vec :: Nat -> Type -> Type -> Type where
  Vec :: Elem a => VSM.MVector s (C a) -> Vec n s a

-- | Alias for single precision mutable matrix
type MMatrixXf  n m s = MMatrix n m s Float
-- | Alias for double precision mutable matrix
type MMatrixXd  n m s = MMatrix n m s Double
-- | Alias for single precision mutable matrix of complex numbers
type MMatrixXcf n m s = MMatrix n m s (Complex Float)
-- | Alias for double precision mutable matrix of complex numbers
type MMatrixXcd n m s = MMatrix n m s (Complex Double)

type IOMatrix n m a   = MMatrix n m RealWorld a
type STMatrix n m s a = MMatrix n m s a

-- | Create a mutable matrix of the given size and fill it with 0 as an initial value.
new :: (PrimMonad p, Elem a, KnownNat n, KnownNat m) => p (MMatrix n m (PrimState p) a)
{-# INLINE new #-}
new = replicate 0

-- | Create a mutable matrix of the given size and fill it with an initial value.
replicate :: forall n m p a. (PrimMonad p, Elem a, KnownNat n, KnownNat m) => a -> p (MMatrix n m (PrimState p) a)
{-# INLINE replicate #-}
replicate !val = do
  let !mm_rows = natToInt @n
      !mm_cols = natToInt @m
      !cval    = toC val
  vals <- VSM.replicate (mm_rows * mm_cols) cval
  pure (MMatrix $! Vec $! vals)

-- | Set all elements of the matrix to a given value.
set :: (PrimMonad p, Elem a) => MMatrix n m (PrimState p) a -> a -> p ()
{-# INLINE set #-}
set (MMatrix (Vec !vec)) !val =
  let !cval = toC val
  in VSM.set vec cval

-- | Copy a matrix.
copy :: (PrimMonad p, Elem a) => MMatrix n m (PrimState p) a -> MMatrix n m (PrimState p) a -> p ()
{-# INLINE copy #-}
copy (MMatrix (Vec m1)) (MMatrix (Vec m2)) = VSM.unsafeCopy m1 m2

-- | Yield the element at the given position.
read :: forall n m p a r c. (PrimMonad p, Elem a, KnownNat n, KnownNat r, KnownNat c, r <= n, c <= m)
  => Row r -> Col c -> MMatrix n m (PrimState p) a -> p a
{-# INLINE read #-}
read _ _ (MMatrix (Vec m)) =
  let !row = natToInt @r
      !col = natToInt @c
      !mm_rows = natToInt @n
  in VSM.unsafeRead m (col * mm_rows + row) >>= \ !val -> let !cval = fromC val in pure cval

-- | Replace the element at the given position.
write :: forall n m p a r c. (PrimMonad p, Elem a, KnownNat n, KnownNat r, KnownNat c, r <= n, c <= m)
  => Row r -> Col c -> MMatrix n m (PrimState p) a -> a -> p ()
{-# INLINE write #-}
write _ _ (MMatrix (Vec m)) !val =
  let !row = natToInt @r
      !col = natToInt @c
      !mm_rows = natToInt @n
      !cval = toC val
  in VSM.unsafeWrite m (col * mm_rows + row) cval

-- | Pass a pointer to the matrix's data to the IO action.
--   Modifying dat through the pointer is unsafe if the matrix
--   could have been frozen before the modification.
unsafeWith :: forall n m a b. (KnownNat n, KnownNat m, Elem a) => IOMatrix n m a -> (Ptr (C a) -> CInt -> CInt -> IO b) -> IO b
{-# INLINE unsafeWith #-}
unsafeWith (MMatrix (Vec m)) f =
  let !cmm_rows = toC $! natToInt @n
      !cmm_cols = toC $! natToInt @m
  in VSM.unsafeWith m $ \p -> f p cmm_rows cmm_cols

vals :: MMatrix n m s a -> VSM.MVector s (C a)
{-# INLINE vals #-}
vals (MMatrix (Vec x)) = x

fromVector :: Elem a => VSM.MVector s (C a) -> MMatrix n m s a
{-# INLINE fromVector #-}
fromVector x = MMatrix (Vec x)
