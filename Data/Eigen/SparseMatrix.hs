{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Eigen.SparseMatrix
  ( SparseMatrix(..)
  , SparseMatrixXf
  , SparseMatrixXd
  , SparseMatrixXcf
  , SparseMatrixXcd
  ) where

import Prelude hiding (read)
import Data.Complex (Complex)
import Data.Kind (Type)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import qualified Foreign.Concurrent as FC
import GHC.TypeLits (Nat, KnownNat, type (<=))
import Data.Eigen.Internal
  ( Elem
  , Cast(..)
  , CSparseMatrix
  , CSparseMatrixPtr
  , natToInt
  , Row
  , Col
  , CTriplet
  )
import qualified Data.Eigen.Internal as Internal

newtype SparseMatrix :: Nat -> Nat -> Type -> Type where
  SparseMatrix :: ForeignPtr (CSparseMatrix a) -> SparseMatrix n m a

-- | Alias for single precision sparse matrix
type SparseMatrixXf  n m = SparseMatrix n m Float
-- | Alias for double precision sparse matrix
type SparseMatrixXd  n m = SparseMatrix n m Double
-- | Alias for single previsiom sparse matrix of complex numbers
type SparseMatrixXcf n m = SparseMatrix n m (Complex Float)
-- | Alias for double prevision sparse matrix of complex numbers
type SparseMatrixXcd n m = SparseMatrix n m (Complex Double)

values :: Elem a => SparseMatrix n m a -> VS.Vector (C a)
values = _getvec Internal.sparse_values

innerIndices :: Elem a => SparseMatrix n m a -> VS.Vector CInt
innerIndices = _getvec Internal.sparse_innerIndices

outerStarts :: Elem a => SparseMatrix n m a -> VS.Vector CInt
outerStarts = _getvec Internal.sparse_outerStarts

--innerNNZs :: Elem a => SparseMatrix n m a -> Maybe (VS.Vector CInt)
rows :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
rows _ = natToInt @n

cols :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
cols _ = natToInt @m

coeff :: forall n m r c a. (Elem a, KnownNat n, KnownNat m, KnownNat r, KnownNat c, r <= n, c <= m)
  => Row r -> Col c -> SparseMatrix n m a -> a
coeff _ _ (SparseMatrix fp) =
  let !c_row = toC $! natToInt @r
      !c_col = toC $! natToInt @c
  in Internal.performIO $ withForeignPtr fp $ \p -> alloca $ \pq -> do
       Internal.call $ Internal.sparse_coeff p c_row c_col pq
       fromC <$> peek pq

(!) :: forall n m r c a. (Elem a, KnownNat n, KnownNat m, KnownNat r, KnownNat c, r <= n, c <= m)
  => SparseMatrix n m a -> (Row r, Col c) -> a
(!) m (row,col) = coeff row col m

{-| For vectors, the l2 norm, and for matrices the Frobenius norm.
    In both cases, it consists in the square root of the sum of the square of all the matrix entries.
    For vectors, this is also equals to the square root of the dot product of this with itself.
-}
norm :: Elem a => SparseMatrix n m a -> a
norm = _unop Internal.sparse_norm (pure . fromC)

-- | For vectors, the squared l2 norm, and for matrices the Frobenius norm. In both cases, it consists in the sum of the square of all the matrix entries. For vectors, this is also equals to the dot product of this with itself.
squaredNorm :: Elem a => SparseMatrix n m a -> a
squaredNorm = _unop Internal.sparse_squaredNorm (pure . fromC)

-- | The l2 norm of the matrix using the Blue's algorithm. A Portable Fortran Program to Find the Euclidean Norm of a Vector, ACM TOMS, Vol 4, Issue 1, 1978.
blueNorm :: Elem a => SparseMatrix n m a -> a
blueNorm = _unop Internal.sparse_blueNorm (pure . fromC)

-- | Extract rectangular block from sparse matrix defined by startRow startCol blockRows blockCols
--block :: I.Elem a b => Int -> Int -> Int -> Int -> SparseMatrix a b -> SparseMatrix a b
--block row col rows cols = _unop (\p pq -> I.sparse_block p (I.cast row) (I.cast col) (I.cast rows) (I.cast cols) pq) _mk

block :: forall sr sc br bc n m a.
     (Elem a, KnownNat sr, KnownNat sc, KnownNat br, KnownNat bc, KnownNat n, KnownNat m)
  => (sr <= n, sc <= m, br <= n, bc <= m)
  => Row sr
  -> Col sc
  -> Row br
  -> Col bc
  -> SparseMatrix n m a
  -> SparseMatrix br bc a
block _ _ _ _ =
  let !c_startRow = toC $! natToInt @sr
      !c_startCol = toC $! natToInt @sc
      !c_rows     = toC $! natToInt @br
      !c_cols     = toC $! natToInt @bc
  in _unop (\p pq -> Internal.sparse_block p c_startRow c_startCol c_rows c_cols pq) _mk

-- | Number of non-zeros elements in the sparse matrix
nonZeros :: Elem a => SparseMatrix n m a -> Int
nonZeros = _unop Internal.sparse_nonZeros (pure . fromC)

-- | The matrix in the compressed format
compress :: Elem a => SparseMatrix n m a -> SparseMatrix n m a
compress = _unop Internal.sparse_makeCompressed _mk

-- | The matrix in the uncompressed mode
uncompress :: Elem a => SparseMatrix n m a -> SparseMatrix n m a
uncompress = _unop Internal.sparse_uncompress _mk

-- | Is this in compressed form?
compressed :: Elem a => SparseMatrix n m a -> Bool
compressed = _unop Internal.sparse_isCompressed (pure . (/=0))

-- | Minor dimension with respect to the storage order
innerSize :: Elem a => SparseMatrix n m a -> Int
innerSize = _unop Internal.sparse_innerSize (pure . fromC)

-- | Major dimension with respect to the storage order
outerSize :: Elem a => SparseMatrix n m a -> Int
outerSize = _unop Internal.sparse_outerSize (pure . fromC)

pruned :: Elem a => a -> SparseMatrix n m a -> SparseMatrix n m a
pruned r = _unop (\p pq -> alloca $ \pr -> poke pr (toC r) >> Internal.sparse_prunedRef p pr pq) _mk

scale :: Elem a => a -> SparseMatrix n m a -> SparseMatrix n m a
scale x = _unop (\p pq -> alloca $ \px -> poke px (toC x) >> Internal.sparse_scale p px pq) _mk

transpose :: Elem a => SparseMatrix n m a -> SparseMatrix m n a
transpose = _unop Internal.sparse_transpose _mk

adjoint :: Elem a => SparseMatrix n m a -> SparseMatrix m n a
adjoint = _unop Internal.sparse_adjoint _mk

add :: Elem a => SparseMatrix n m a -> SparseMatrix n m a -> SparseMatrix n m a
add = _binop Internal.sparse_add _mk

sub :: Elem a => SparseMatrix n m a -> SparseMatrix n m a -> SparseMatrix n m a
sub = _binop Internal.sparse_sub _mk

mul :: Elem a => SparseMatrix p q a -> SparseMatrix q r a -> SparseMatrix p r a
mul = _binop Internal.sparse_mul _mk

toVector :: Elem a => SparseMatrix n m a -> VS.Vector (CTriplet (C a))
toVector m@(SparseMatrix fp) = Internal.performIO $ do
  let !size = nonZeros m
  tris <- VSM.new size
  withForeignPtr fp $ \p ->
    VSM.unsafeWith tris $ \q ->
      Internal.call $ Internal.sparse_toList p q (toC size)
  VS.unsafeFreeze tris

_unop :: Storable b => (CSparseMatrixPtr a -> Ptr b -> IO CString) -> (b -> IO c) -> SparseMatrix n m a -> c
_unop f g (SparseMatrix fp) = Internal.performIO $
  withForeignPtr fp $ \p ->
    alloca $ \pq -> do
      Internal.call (f p pq)
      peek pq >>= g

_binop :: Storable b => (CSparseMatrixPtr a -> CSparseMatrixPtr a -> Ptr b -> IO CString) -> (b -> IO c) -> SparseMatrix n m a -> SparseMatrix n1 m1 a -> c
_binop f g (SparseMatrix fp1) (SparseMatrix fp2) = Internal.performIO $
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
      alloca $ \pq -> do
        Internal.call (f p1 p2 pq)
        peek pq >>= g

_getvec :: (Elem a, Storable b) => (Ptr (CSparseMatrix a) -> Ptr CInt -> Ptr (Ptr b) -> IO CString) -> SparseMatrix n m a -> VS.Vector b
_getvec f (SparseMatrix fm) = Internal.performIO $
  withForeignPtr fm $ \m ->
    alloca $ \ps ->
      alloca $ \pq -> do
        Internal.call $ f m ps pq
        s <- fromIntegral <$> peek ps
        q <- peek pq
        fr <- FC.newForeignPtr q $ touchForeignPtr fm
        pure $! VS.unsafeFromForeignPtr0 fr s

_clone :: Elem a => ForeignPtr (CSparseMatrix a) -> IO (ForeignPtr (CSparseMatrix a))
_clone fp = withForeignPtr fp $ \p -> alloca $ \pq -> do
  Internal.call $ Internal.sparse_clone p pq
  q <- peek pq
  FC.newForeignPtr q $ Internal.call $ Internal.sparse_free q

_mk :: Elem a => Ptr (CSparseMatrix a) -> IO (SparseMatrix n m a)
_mk p = SparseMatrix <$> FC.newForeignPtr p (Internal.call $ Internal.sparse_free p)

--values :: Elem a => SparseMatrix n m a -> VS.Vector (C a)
--values (SparseMatrix v) = v

--innerIndices :: Elem a => SparseMatrix n m a -> VS.Vector CInt
--inner
