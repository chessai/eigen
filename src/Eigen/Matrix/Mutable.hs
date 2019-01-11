{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
#endif

module Eigen.Matrix.Mutable
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

import Eigen.Internal
  ( Elem
  , C(..)
  , natToInt
  )

import Control.Monad.Primitive (PrimMonad(..))
import Data.Complex (Complex)
import Data.Kind (Type)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import GHC.Exts (RealWorld)
import GHC.TypeLits (Nat, type (*), KnownNat)
import Prelude hiding (replicate, read)
import Refined
import qualified Data.Vector.Storable.Mutable as VSM

-- | A mutable matrix. See 'Eigen.Matrix.Matrix' for
--   details about matrix layout.
newtype MMatrix :: Nat -> Nat -> Type -> Type -> Type where
  MMatrix :: Vec (n * m) s a -> MMatrix n m s a

-- | Used internally to track the size and corresponding C type of the matrix.
newtype Vec :: Nat -> Type -> Type -> Type where
  Vec :: VSM.MVector s (C a) -> Vec n s a

-- | Alias for single precision mutable matrix
type MMatrixXf  n m s = MMatrix n m s Float
-- | Alias for double precision mutable matrix
type MMatrixXd  n m s = MMatrix n m s Double
-- | Alias for single precision mutable matrix of complex numbers
type MMatrixXcf n m s = MMatrix n m s (Complex Float)
-- | Alias for double precision mutable matrix of complex numbers
type MMatrixXcd n m s = MMatrix n m s (Complex Double)

-- | A mutable matrix where the state token is specialised to 'RealWorld'.
type IOMatrix n m a   = MMatrix n m RealWorld a
-- | This type does not differ from MSparseMatrix, but might be desirable for readability.
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
  _vals <- VSM.replicate (mm_rows * mm_cols) cval
  pure (MMatrix $! Vec $! _vals)

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
read :: forall n m p a. (PrimMonad p, Elem a, KnownNat n, KnownNat m)
  => Refined (FromTo 0 n) Int -> Refined (FromTo 0 m) Int -> MMatrix n m (PrimState p) a -> p a
{-# INLINE read #-}
read r c (MMatrix (Vec m)) =
  let !row = unrefine r
      !col = unrefine c
      !mm_rows = natToInt @n
  in VSM.unsafeRead m (col * mm_rows + row) >>= \ !val -> let !cval = fromC val in pure cval

-- | Replace the element at the given position.
write :: forall n m p a. (PrimMonad p, Elem a, KnownNat n, KnownNat m)
  => Refined (FromTo 0 n) Int -> Refined (FromTo 0 m) Int -> MMatrix n m (PrimState p) a -> a -> p ()
{-# INLINE write #-}
write r c (MMatrix (Vec m)) !val =
  let !row = unrefine r
      !col = unrefine c
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

-- | Return a mutable storable 'VSM.MVector' of the corresponding C types to one's mutable matrix.
vals :: MMatrix n m s a -> VSM.MVector s (C a)
{-# INLINE vals #-}
vals (MMatrix (Vec x)) = x

-- | Create a mutable matrix from a mutable storable 'VSM.MVector'.
fromVector :: Elem a => VSM.MVector s (C a) -> MMatrix n m s a
{-# INLINE fromVector #-}
fromVector x = MMatrix (Vec x)
