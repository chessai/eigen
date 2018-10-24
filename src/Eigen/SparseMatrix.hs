{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Eigen.SparseMatrix where

import Control.Monad (when, guard)
import Control.Monad.Primitive (PrimMonad(..), unsafePrimToPrim)
import qualified Prelude
import Prelude hiding (read)
import Data.Binary (Binary(..))
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BSL
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
import Eigen.Internal
  ( Elem
  , Cast(..)
  , CSparseMatrix
  , CSparseMatrixPtr
  , natToInt
  , Row(..)
  , Col(..)
  , CTriplet(..)
  )
import qualified Data.List as List
import qualified Eigen.Internal as Internal
import qualified Eigen.Matrix as M
import qualified Eigen.Matrix.Mutable as MM
import qualified Eigen.SparseMatrix.Mutable as SMM

{-| A versatible sparse matrix representation.
SparseMatrix is the main sparse matrix representation of Eigen's sparse module.
It offers high performance and low memory usage.
It implements a more versatile variant of the widely-used Compressed Column (or Row) Storage scheme.
It consists of four compact arrays:
* `values`: stores the coefficient values of the non-zeros.
* `innerIndices`: stores the row (resp. column) indices of the non-zeros.
* `outerStarts`: stores for each column (resp. row) the index of the first non-zero in the previous two arrays.
* `innerNNZs`: stores the number of non-zeros of each column (resp. row). The word inner refers to an inner vector that is a column for a column-major matrix, or a row for a row-major matrix. The word outer refers to the other direction.
This storage scheme is better explained on an example. The following matrix
@
0   3   0   0   0
22  0   0   0   17
7   5   0   1   0
0   0   0   0   0
0   0   14  0   8
@
and one of its possible sparse, __column major__ representation:
@
values:         22  7   _   3   5   14  _   _   1   _   17  8
innerIndices:   1   2   _   0   2   4   _   _   2   _   1   4
outerStarts:    0   3   5   8   10  12
innerNNZs:      2   2   1   1   2
@
Currently the elements of a given inner vector are guaranteed to be always sorted by increasing inner indices.
The "\_" indicates available free space to quickly insert new elements. Assuming no reallocation is needed,
the insertion of a random element is therefore in @O(nnz_j)@ where @nnz_j@ is the number of nonzeros of the
respective inner vector. On the other hand, inserting elements with increasing inner indices in a given inner
vector is much more efficient since this only requires to increase the respective `innerNNZs` entry that is a @O(1)@ operation.
The case where no empty space is available is a special case, and is refered as the compressed mode.
It corresponds to the widely used Compressed Column (or Row) Storage schemes (CCS or CRS).
Any `SparseMatrix` can be turned to this form by calling the `compress` function.
In this case, one can remark that the `innerNNZs` array is redundant with `outerStarts` because we the equality:
@InnerNNZs[j] = OuterStarts[j+1]-OuterStarts[j]@. Therefore, in practice a call to `compress` frees this buffer.
The results of Eigen's operations always produces compressed sparse matrices.
On the other hand, the insertion of a new element into a `SparseMatrix` converts this later to the uncompressed mode.
For more infomration please see Eigen <http://eigen.tuxfamily.org/dox/classEigen_1_1SparseMatrix.html documentation page>.
-}
newtype SparseMatrix :: Nat -> Nat -> Type -> Type where
  SparseMatrix :: ForeignPtr (CSparseMatrix a) -> SparseMatrix n m a

instance forall n m a. (Elem a, Show a, KnownNat n, KnownNat m) => Show (SparseMatrix n m a) where
  show m = concat
    [ "SparseMatrix"
    , show (rows_ m)
    , "x"
    , show (cols_ m)
    , "\n"
    , List.intercalate "\n" $ Prelude.map (List.intercalate "\t" . Prelude.map show) $ toDenseList m
    , "\n"
    ]

instance forall n m a. (Elem a, KnownNat n, KnownNat m) => Binary (SparseMatrix n m a) where
  put mat = do
    put $ Internal.magicCode (undefined :: C a)
    put $ natToInt @n
    put $ natToInt @m
    put $ toVector mat
  
  get = do
    get >>= (`when` fail "wrong matrix type") . (/= Internal.magicCode (undefined :: C a))
    fromVector <$> get

-- | Encode the sparse matrix as a lazy bytestring
encode :: (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> BSL.ByteString
encode = Binary.encode

-- | Decode the sparse matrix from a lazy bytestring
decode :: (Elem a, KnownNat n, KnownNat m) => BSL.ByteString -> SparseMatrix n m a
decode = Binary.decode

-- | Alias for single precision sparse matrix
type SparseMatrixXf  n m = SparseMatrix n m Float
-- | Alias for double precision sparse matrix
type SparseMatrixXd  n m = SparseMatrix n m Double
-- | Alias for single previsiom sparse matrix of complex numbers
type SparseMatrixXcf n m = SparseMatrix n m (Complex Float)
-- | Alias for double prevision sparse matrix of complex numbers
type SparseMatrixXcd n m = SparseMatrix n m (Complex Double)

-- | Get the coefficient values of the non-zeros.
values :: Elem a => SparseMatrix n m a -> VS.Vector a
values = VS.map fromC . _getvec Internal.sparse_values

-- | Get the row indices of the non-zeros.
innerIndices :: Elem a => SparseMatrix n m a -> VS.Vector Int
innerIndices = VS.map fromC . _getvec Internal.sparse_innerIndices

-- | Gets for each column the index of the first non-zero in the previous two arrays.
outerStarts :: Elem a => SparseMatrix n m a -> VS.Vector Int
outerStarts = VS.map fromC . _getvec Internal.sparse_outerStarts

-- | Gets the number of non-zeros of each column.
-- The word inner refers to an inner vector that is a column for a column-major matrix, or a row for a row-major matrix.
-- The word outer refers to the other direction
innerNNZs :: Elem a => SparseMatrix n m a -> Maybe (VS.Vector Int)
innerNNZs m
  | compressed m = Nothing
  | otherwise    = Just $ VS.map fromC $ _getvec Internal.sparse_innerNNZs m

-- | Number of rows in the sparse matrix
rows :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Row n
rows _ = Row @n

-- | Number of colums in the sparse matrix
cols :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Col m
cols _ = Col @m

-- | Number of rows in the sparse matrix
rows_ :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
rows_ _ = natToInt @n

-- | Number of colums in the sparse matrix
cols_ :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
cols_ _ = natToInt @m


-- | Sparse matrix coefficient at the given row and column
coeff :: forall n m r c a. (Elem a, KnownNat n, KnownNat m, KnownNat r, KnownNat c, r <= n, c <= m)
  => Row r -> Col c -> SparseMatrix n m a -> a
coeff _ _ (SparseMatrix fp) =
  let !c_row = toC $! natToInt @r
      !c_col = toC $! natToInt @c
  in Internal.performIO $ withForeignPtr fp $ \p -> alloca $ \pq -> do
       Internal.call $ Internal.sparse_coeff p c_row c_col pq
       fromC <$> peek pq

-- | Matrix coefficient at the given row and column
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

-- | Extract a rectangular block from the sparse matrix, given a startRow, startCol, blockRows, blockCols
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

-- | Number of elements in the sparse matrix, including zeros
elems :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
elems _ = (natToInt @n) * (natToInt @m)

-- | The matrix in the compressed format
compress :: Elem a => SparseMatrix n m a -> SparseMatrix n m a
compress = _unop Internal.sparse_makeCompressed _mk

-- | The matrix in the uncompressed format
uncompress :: Elem a => SparseMatrix n m a -> SparseMatrix n m a
uncompress = _unop Internal.sparse_uncompress _mk

-- | Is the matrix compressed?
compressed :: Elem a => SparseMatrix n m a -> Bool
compressed = _unop Internal.sparse_isCompressed (pure . (/=0))

-- | Minor dimension with respect to the storage order
innerSize :: Elem a => SparseMatrix n m a -> Int
innerSize = _unop Internal.sparse_innerSize (pure . fromC)

-- | Major dimension with respect to the storage order
outerSize :: Elem a => SparseMatrix n m a -> Int
outerSize = _unop Internal.sparse_outerSize (pure . fromC)

-- | Suppresses all nonzeros which are much smaller than the reference under the tolerance @epsilon@
pruned :: Elem a => a -> SparseMatrix n m a -> SparseMatrix n m a
pruned r = _unop (\p pq -> alloca $ \pr -> poke pr (toC r) >> Internal.sparse_prunedRef p pr pq) _mk

-- | Multiply matrix on a given scalar
scale :: Elem a => a -> SparseMatrix n m a -> SparseMatrix n m a
scale x = _unop (\p pq -> alloca $ \px -> poke px (toC x) >> Internal.sparse_scale p px pq) _mk

-- | Transpose of the sparse matrix
transpose :: Elem a => SparseMatrix n m a -> SparseMatrix m n a
transpose = _unop Internal.sparse_transpose _mk

-- | Adjoint of the sparse matrix
adjoint :: Elem a => SparseMatrix n m a -> SparseMatrix m n a
adjoint = _unop Internal.sparse_adjoint _mk

-- | Add two sparse matrices by adding the corresponding entries together.
add :: Elem a => SparseMatrix n m a -> SparseMatrix n m a -> SparseMatrix n m a
add = _binop Internal.sparse_add _mk

-- | Subtract two sparse matrices by subtracting the corresponding entries together.
sub :: Elem a => SparseMatrix n m a -> SparseMatrix n m a -> SparseMatrix n m a
sub = _binop Internal.sparse_sub _mk

-- | Matrix multiplication.
mul :: Elem a => SparseMatrix p q a -> SparseMatrix q r a -> SparseMatrix p r a
mul = _binop Internal.sparse_mul _mk

map :: (Elem a, Elem b, KnownNat n, KnownNat m) => (a -> b) -> SparseMatrix n m a -> SparseMatrix n m b
map f m = fromVector . VS.map g . toVector $ m where
  g (CTriplet r c v) = CTriplet r c $ (toC . f . fromC) v

imap :: (Elem a, Elem b, KnownNat n, KnownNat m) => (Int -> Int -> a -> b) -> SparseMatrix n m a -> SparseMatrix n m b
imap f m = fromVector . VS.map g . toVector $ m where
  g (CTriplet r c v) =
    CTriplet r c $ toC $ f (fromC r) (fromC c) (fromC v)

-- | Construct asparse matrix of the given size from the storable vector of triplets (row, col, val)
fromVector :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => VS.Vector (CTriplet a)
  -> SparseMatrix n m a
fromVector tris =
  let !c_rs = toC $! natToInt @n
      !c_cs = toC $! natToInt @m
      !len  = toC $! VS.length tris
  in Internal.performIO $ VS.unsafeWith tris $ \p ->
       alloca $ \pq -> do
         Internal.call $ Internal.sparse_fromList c_rs c_cs p len pq
         peek pq >>= _mk

-- | Convert a sparse matrix to the list of triplets (row, col, val). Compressed elements will not be included.
toVector :: Elem a => SparseMatrix n m a -> VS.Vector (CTriplet a)
toVector m@(SparseMatrix fp) = Internal.performIO $ do
  let !size = nonZeros m
  tris <- VSM.new size
  withForeignPtr fp $ \p ->
    VSM.unsafeWith tris $ \q ->
      Internal.call $ Internal.sparse_toList p q (toC size)
  VS.unsafeFreeze tris

-- | Convert a sparse matrix to the list of triplets (row, col, val). Compressed elements will not be included.
toList :: Elem a => SparseMatrix n m a -> [(Int, Int, a)]
toList = Prelude.map fromC . VS.toList . toVector

-- | Construct a sparse matrix from a list of triples (row, val, col)
--
fromList :: (Elem a, KnownNat n, KnownNat m) => [(Int, Int, a)] -> SparseMatrix n m a
fromList = fromVector . VS.fromList . fmap toC

-- | Convert a sparse matrix to a (n X m) dense list of values
toDenseList :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> [[a]]
toDenseList mat = [[_unsafeCoeff row col mat | col <- [0 .. _unsafeCols mat - 1]] | row <- [0 .. _unsafeRows mat - 1]]

fromDenseList :: forall n m a. (Elem a, Eq a, KnownNat n, KnownNat m) => [[a]] -> Maybe (SparseMatrix n m a)
fromDenseList list =
  let _rows = List.length list
      _cols = List.foldl' max 0 $ List.map length list
  in if ((_rows /= (natToInt @n)) || (_cols /= (natToInt @m)))
    then Nothing
    else Just $ fromList $ do
      (row, vals) <- zip [0..] list
      (col, val) <- zip [0..] vals
      guard $ val /= 0
      return (row, col, val)
      
  
--  fromList $ do
-- (row, vals) <- zip [0..] list
-- (col, val) <- zip [0..] vals
-- guard $ val /= 0
-- return (row, col, val)
-- where
--   rows = List.length list
--   cols = List.foldl' max 0 $ List.map length list

-- | Construct a dense matrix from a sparse matrix
toMatrix :: (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> M.Matrix n m a
toMatrix (SparseMatrix fp) = Internal.performIO $ do
  m0 :: MM.IOMatrix n m a <- MM.new
  MM.unsafeWith m0 $ \_vals _rows _cols ->
    withForeignPtr fp $ \pm1 ->
      Internal.call $ Internal.sparse_toMatrix pm1 _vals _rows _cols
  M.unsafeFreeze m0

-- | Construct a sparse matrix from a dense matrix. zero-elements will be compressed.
fromMatrix :: (Elem a, KnownNat n, KnownNat m) => M.Matrix n m a -> SparseMatrix n m a
fromMatrix m1 = Internal.performIO $ alloca $ \pm0 ->
  M.unsafeWith m1 $ \_vals _rows _cols -> do
    Internal.call $ Internal.sparse_fromMatrix _vals _rows _cols pm0
    peek pm0 >>= _mk

-- | Yield an immutable copy of the mutable matrix
freeze :: (Elem a, PrimMonad p) => SMM.MSparseMatrix n m (PrimState p) a -> p (SparseMatrix n m a)
freeze (SMM.MSparseMatrix fp) = SparseMatrix <$> _clone fp

-- | Yield a mutable copy of the immutable matrix.
thaw :: (Elem a, PrimMonad p) => SparseMatrix n m a -> p (SMM.MSparseMatrix n m (PrimState p) a)
thaw (SparseMatrix fp) = SMM.MSparseMatrix <$> _clone fp

-- | Unsafely convert a mutable matrix to an immutable one without copying. The mutable matrix may not be used after this operation.
unsafeFreeze :: (Elem a, PrimMonad p) => SMM.MSparseMatrix n m (PrimState p) a -> p (SparseMatrix n m a) 
unsafeFreeze (SMM.MSparseMatrix fp) = return $! SparseMatrix fp

-- | Unsafely convert an immutable matrix to a mutable one without copying. The immutable matrix may not be used after this operation.
unsafeThaw :: (Elem a, PrimMonad p) => SparseMatrix n m a -> p (SMM.MSparseMatrix n m (PrimState p) a) 
unsafeThaw (SparseMatrix fp) = return $! SMM.MSparseMatrix fp

getRow :: forall n m r a. (Elem a, KnownNat n, KnownNat m, KnownNat r, r <= n, 1 <= n) => Row r -> SparseMatrix n m a -> SparseMatrix 1 m a
getRow row mat = block row (Col @0) (Row @1) (Col @m) mat

getCol :: forall n m c a. (Elem a, KnownNat n, KnownNat m, KnownNat c, c <= m, 1 <= m) => Col c -> SparseMatrix n m a -> SparseMatrix n 1 a
getCol col mat = block (Row @0) col (Row @n) (Col @1) mat

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

_clone :: (PrimMonad p, Elem a) => ForeignPtr (CSparseMatrix a) -> p (ForeignPtr (CSparseMatrix a))
_clone fp = unsafePrimToPrim $ withForeignPtr fp $ \p -> alloca $ \pq -> do
  Internal.call $ Internal.sparse_clone p pq
  q <- peek pq
  FC.newForeignPtr q $ Internal.call $ Internal.sparse_free q

_mk :: Elem a => Ptr (CSparseMatrix a) -> IO (SparseMatrix n m a)
_mk p = SparseMatrix <$> FC.newForeignPtr p (Internal.call $ Internal.sparse_free p)

-- | Number of rows in the sparse matrix
_unsafeRows :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
{-# INLINE _unsafeRows #-}
_unsafeRows _ = natToInt @n

-- | Number of colums in the sparse matrix
_unsafeCols :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
{-# INLINE _unsafeCols #-}
_unsafeCols _ = natToInt @m

_unsafeCoeff :: (Elem a, KnownNat n) => Int -> Int -> SparseMatrix n m a -> a
{-# INLINE _unsafeCoeff #-}
_unsafeCoeff !row !col (SparseMatrix fp) =
  let !c_row = toC row
      !c_col = toC col
  in Internal.performIO $ withForeignPtr fp $ \p -> alloca $ \pq -> do
       Internal.call $ Internal.sparse_coeff p c_row c_col pq
       fromC <$> peek pq
