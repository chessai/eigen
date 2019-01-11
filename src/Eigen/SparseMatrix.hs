{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}

module Eigen.SparseMatrix
  ( -- * Types
    SparseMatrix(..)
  , SparseMatrixXf
  , SparseMatrixXd
  , SparseMatrixXcf
  , SparseMatrixXcd

    -- * Matrix internal data
  , elems
  , values
  , innerIndices
  , outerStarts
  , innerNNZs

    -- * Accessors
  , cols
  , rows
  , cols'
  , rows'
  , coeff
  , (!)
  , getRow
  , getCol
  , getRows
  , getRows'
  , getCols
  , getCols'
  , fromRows
  , fromCols

    -- * Matrix conversions
  , fromList
  , toList'
  , fromVector
  , toVector 
  , toVector'
  , fromVectorSized
  , fromListSized
  , fromDenseList
  , toDenseList'
  , toDenseList
  , fromMatrix
  , toMatrix

    -- * Matrix properties
  , norm
  , squaredNorm
  , blueNorm
  , block
  , nonZeros
  , innerSize
  , outerSize

    -- * Basic matrix algebra
  , add
  , sub
  , mul

    -- * Matrix tranformations
  , pruned
  , scale
  , transpose
  , adjoint
  , map
  , imap
  , diagCol
  , diagRow
  , squareSubset

    -- * Matrix production
  , empty 
  , constant
  , ones
  , identity

    -- * Matrix consumption
  , sum
  , product

    -- ** Matrix folds
  , foldMap

    -- * Matrix representation
  , compress
  , uncompress
  , compressed

    -- * Mutable matrices
  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze
  
  , StorableVectorSizeEqualTo 
  ) where

import Control.Monad (guard, unless)
import Control.Monad.Primitive (PrimMonad(..), unsafePrimToPrim)
import Data.Complex (Complex)
import Data.Kind (Type)
import Data.Monoid (Sum(..), Product(..))
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Typeable (typeOf)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.TypeLits (Nat, KnownNat, type (<=), type (*), natVal)
import Prelude hiding (read, map, foldMap, sum, product)
import Refined
import qualified Data.List as List
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Foreign.Concurrent as FC
import qualified Prelude

import qualified Eigen.Internal as Internal
import qualified Eigen.Matrix as M
import qualified Eigen.Matrix.Mutable as MM
import qualified Eigen.SparseMatrix.Mutable as SMM

import Eigen.Internal
  ( Elem
  , Cast(..)
  , CSparseMatrix
  , CSparseMatrixPtr
  , natToInt
  , _unsafeRefine
  , CTriplet(..)
  )

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
    , show (rows m)
    , "x"
    , show (cols m)
    , "\n"
    , List.intercalate "\n" $ Prelude.map (List.intercalate "\t" . Prelude.map show) $ toDenseList m
    , "\n"
    ]

-- | A Predicate that ensures a 'VS.Vector' has
--   a number of elements equivalent to the given
--   type-level natural.
data StorableVectorSizeEqualTo (n :: Nat)
  deriving (Generic)

instance (KnownNat n, Storable a) => Predicate (StorableVectorSizeEqualTo n) (VS.Vector a) where
  validate p x = do
    let x' = natVal p
        sz = VS.length x
    unless (sz == fromIntegral x') $ do
      throwRefineOtherException (typeOf p)
        $ "Size of the vector is not equal to " <> fromString (show x') <> "\n"
        <> "  Size is: " <> fromString (show sz)

-- | Alias for single precision sparse matrix
type SparseMatrixXf  n m = SparseMatrix n m Float
-- | Alias for double precision sparse matrix
type SparseMatrixXd  n m = SparseMatrix n m Double
-- | Alias for single previsiom sparse matrix of complex numbers
type SparseMatrixXcf n m = SparseMatrix n m (Complex Float)
-- | Alias for double prevision sparse matrix of complex numbers
type SparseMatrixXcd n m = SparseMatrix n m (Complex Double)

empty :: (Elem a) => SparseMatrix 0 0 a
{-# INLINE empty #-}
empty = fromMatrix M.empty

-- | The identity matrix.
identity :: (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a
{-# INLINE identity #-}
identity = fromMatrix M.identity

-- | Matrix where all coeffs are filled with the given value
constant :: (Elem a, KnownNat n, KnownNat m) => a -> SparseMatrix n m a
{-# INLINE constant #-}
constant = fromMatrix . M.constant

-- | Matrix where all coeffs are one.
ones :: (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a
{-# INLINE ones #-}
ones = constant 1

-- | Get the coefficient values of the non-zeros.
values :: Elem a => SparseMatrix n m a -> VS.Vector a
{-# INLINE values #-}
values = VS.map fromC . _getvec Internal.sparse_values

-- | Get the row indices of the non-zeros.
innerIndices :: Elem a => SparseMatrix n m a -> VS.Vector Int
{-# INLINE innerIndices #-}
innerIndices = VS.map fromC . _getvec Internal.sparse_innerIndices

-- | Gets for each column the index of the first non-zero in the previous two arrays.
outerStarts :: Elem a => SparseMatrix n m a -> VS.Vector Int
{-# INLINE outerStarts #-}
outerStarts = VS.map fromC . _getvec Internal.sparse_outerStarts

-- | Gets the number of non-zeros of each column.
-- The word inner refers to an inner vector that is a column for a column-major matrix, or a row for a row-major matrix.
-- The word outer refers to the other direction.
innerNNZs :: Elem a => SparseMatrix n m a -> Maybe (VS.Vector Int)
{-# INLINEABLE innerNNZs #-}
innerNNZs m
  | compressed m = Nothing
  | otherwise    = Just $ VS.map fromC $ _getvec Internal.sparse_innerNNZs m

-- | Number of rows in the sparse matrix, wrapped in a refinement.
rows' :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Refined (EqualTo n) Int
{-# INLINE rows' #-}
rows' = _unsafeRefine . rows

-- | Number of columns in the sparse matrix, wrapped in a refinement.
cols' :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Refined (EqualTo m) Int
{-# INLINE cols' #-}
cols' = _unsafeRefine . cols

-- | Number of rows in the sparse matrix.
rows :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
{-# INLINE rows #-}
rows _ = natToInt @n

-- | Number of colums in the sparse matrix.
cols :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
{-# INLINE cols #-}
cols _ = natToInt @m

-- | Sparse matrix coefficient at the given row and column
coeff :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => Refined (FromTo 0 n) Int
  -> Refined (FromTo 0 m) Int
  -> SparseMatrix n m a
  -> a
{-# INLINE coeff #-}
coeff rr rc (SparseMatrix fp) =
  let !c_row = toC $! unrefine rr
      !c_col = toC $! unrefine rc
  in Internal.performIO $ withForeignPtr fp $ \p -> alloca $ \pq -> do
       Internal.call $ Internal.sparse_coeff p c_row c_col pq
       fromC <$> peek pq

-- | Matrix coefficient at the given row and column
(!) :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => SparseMatrix n m a
  -> (Refined (FromTo 0 n) Int, Refined (FromTo 0 m) Int)
  -> a
{-# INLINE (!) #-}
(!) m (row,col) = coeff row col m

{-| For vectors, the l2 norm, and for matrices the Frobenius norm.
    In both cases, it consists in the square root of the sum of the square of all the matrix entries.
    For vectors, this is also equals to the square root of the dot product of this with itself.
-}
norm :: Elem a => SparseMatrix n m a -> a
{-# INLINE norm #-}
norm = _unop Internal.sparse_norm (pure . fromC)

-- | For vectors, the squared l2 norm, and for matrices the Frobenius norm. In both cases, it consists in the sum of the square of all the matrix entries. For vectors, this is also equals to the dot product of this with itself.
squaredNorm :: Elem a => SparseMatrix n m a -> a
{-# INLINE squaredNorm #-}
squaredNorm = _unop Internal.sparse_squaredNorm (pure . fromC)

-- | The l2 norm of the matrix using the Blue's algorithm. A Portable Fortran Program to Find the Euclidean Norm of a Vector, ACM TOMS, Vol 4, Issue 1, 1978.
blueNorm :: Elem a => SparseMatrix n m a -> a
{-# INLINE blueNorm #-}
blueNorm = _unop Internal.sparse_blueNorm (pure . fromC)

block :: forall n m a br bc. (Elem a, KnownNat n, KnownNat m, KnownNat br, KnownNat bc, br <= n, bc <= m)
  => Refined (FromTo 0 n) Int -- ^ starting row
  -> Refined (FromTo 0 m) Int -- ^ starting col
  -> Proxy br                 -- ^ block of rows
  -> Proxy bc                 -- ^ block of cols
  -> SparseMatrix n m a       -- ^ extract from this
  -> SparseMatrix br bc a     -- ^ extraction
{-# INLINE block #-}
block sr sc _ _ =
  let !c_startRow = toC $! unrefine sr
      !c_startCol = toC $! unrefine sc
      !c_rows     = toC $! natToInt @br
      !c_cols     = toC $! natToInt @bc
  in _unop (\p pq -> Internal.sparse_block p c_startRow c_startCol c_rows c_cols pq) _mk

-- | Number of non-zeros elements in the sparse matrix
nonZeros :: Elem a => SparseMatrix n m a -> Int
{-# INLINE nonZeros #-}
nonZeros = _unop Internal.sparse_nonZeros (pure . fromC)

-- | Number of elements in the sparse matrix, including zeros
elems :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Int
{-# INLINE elems #-}
elems _ = (natToInt @n) * (natToInt @m)

-- | The matrix in the compressed format
compress :: Elem a => SparseMatrix n m a -> SparseMatrix n m a
{-# INLINE compress #-}
compress = _unop Internal.sparse_makeCompressed _mk

-- | The matrix in the uncompressed format
uncompress :: Elem a => SparseMatrix n m a -> SparseMatrix n m a
{-# INLINE uncompress #-}
uncompress = _unop Internal.sparse_uncompress _mk

-- | Is the matrix compressed?
compressed :: Elem a => SparseMatrix n m a -> Bool
{-# INLINE compressed #-}
compressed = _unop Internal.sparse_isCompressed (pure . (/=0))

-- | Minor dimension with respect to the storage order
innerSize :: Elem a => SparseMatrix n m a -> Int
{-# INLINE innerSize #-}
innerSize = _unop Internal.sparse_innerSize (pure . fromC)

-- | Major dimension with respect to the storage order
outerSize :: Elem a => SparseMatrix n m a -> Int
{-# INLINE outerSize #-}
outerSize = _unop Internal.sparse_outerSize (pure . fromC)

-- | Suppresses all nonzeros which are much smaller than the reference under the tolerance @epsilon@
pruned :: Elem a => a -> SparseMatrix n m a -> SparseMatrix n m a
{-# INLINE pruned #-}
pruned r = _unop (\p pq -> alloca $ \pr -> poke pr (toC r) >> Internal.sparse_prunedRef p pr pq) _mk

-- | Multiply matrix on a given scalar
scale :: Elem a => a -> SparseMatrix n m a -> SparseMatrix n m a
{-# INLINE scale #-}
scale x = _unop (\p pq -> alloca $ \px -> poke px (toC x) >> Internal.sparse_scale p px pq) _mk

-- | Transpose of the sparse matrix
transpose :: Elem a => SparseMatrix n m a -> SparseMatrix m n a
{-# INLINE transpose #-}
transpose = _unop Internal.sparse_transpose _mk

-- | Adjoint of the sparse matrix
adjoint :: Elem a => SparseMatrix n m a -> SparseMatrix m n a
{-# INLINE adjoint #-}
adjoint = _unop Internal.sparse_adjoint _mk

-- | Add all of the elements of the matrix together.
sum :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> a
{-# INLINE sum #-}
sum = getSum . foldMap Sum

product :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> a
{-# INLINE product #-}
product = getProduct . foldMap Product

foldMap :: (Elem a, KnownNat n, KnownNat m, Monoid s) => (a -> s) -> SparseMatrix n m a -> s
{-# INLINE foldMap #-}
foldMap f = VS.foldl' (\a (CTriplet _ _ b) -> a `mappend` f (fromC b)) mempty . toVector
 
-- | Add two sparse matrices by adding the corresponding entries together.
add :: Elem a => SparseMatrix n m a -> SparseMatrix n m a -> SparseMatrix n m a
{-# INLINE add #-}
add = _binop Internal.sparse_add _mk

-- | Subtract two sparse matrices by subtracting the corresponding entries together.
sub :: Elem a => SparseMatrix n m a -> SparseMatrix n m a -> SparseMatrix n m a
{-# INLINE sub #-}
sub = _binop Internal.sparse_sub _mk

-- | Matrix multiplication.
mul :: Elem a => SparseMatrix p q a -> SparseMatrix q r a -> SparseMatrix p r a
{-# INLINE mul #-}
mul = _binop Internal.sparse_mul _mk

-- | Map a function over the 'SparseMatrix'.
map :: (Elem a, Elem b, KnownNat n, KnownNat m) => (a -> b) -> SparseMatrix n m a -> SparseMatrix n m b
{-# INLINE map #-}
map f m = go m where
  go = fromVector . _unsafeRefine . VS.map g . toVector
  g (CTriplet r c v) = CTriplet r c $ (toC . f . fromC) v

-- | Map an indexed function over the 'SparseMatrix'.
imap :: (Elem a, Elem b, KnownNat n, KnownNat m) => (Int -> Int -> a -> b) -> SparseMatrix n m a -> SparseMatrix n m b
{-# INLINE imap #-}
imap f m = go m where
  go = fromVector . _unsafeRefine . VS.map g . toVector
  g (CTriplet r c v) =
    let !_r = fromC r
        !_c = fromC c
        !_v = fromC v
    in CTriplet r c $ toC $ f _r _c _v

-- | Transform a column into a diagonal matrix.
diagCol :: forall n m a. (Elem a, KnownNat n, KnownNat m, 1 <= m)
  => Refined (FromTo 0 m) Int
  -> SparseMatrix n m a
  -> SparseMatrix n n a
{-# INLINEABLE diagCol #-}
diagCol col_ = fromListSized n n . _unsafeRefine . fmap (\(i,_,v) -> (i,i,v)) . toList . getCol col_
  where
    n = _unsafeRefine (natToInt @n)

-- | Transform a row into a diagonal matrix.
diagRow :: forall n m a. (Elem a, KnownNat n, KnownNat m, 1 <= n)
  => Refined (FromTo 0 n) Int
  -> SparseMatrix n m a
  -> SparseMatrix m m a
{-# INLINEABLE diagRow #-}
diagRow row_ = fromListSized m m . _unsafeRefine . fmap (\(_,j,v) -> (j,j,v)) . toList . getRow row_
  where m = _unsafeRefine (natToInt @m)

-- | Return a (not necessarily square) subset of a square matrix.
squareSubset :: forall n n1 m1 a. (Elem a, KnownNat n, KnownNat n1, KnownNat m1, n1 <= n, m1 <= n, 1 <= n)
  => Refined (SizeEqualTo (n1 * m1)) [Refined (FromTo 0 n) Int] -- ^ The list of rows and columns to access.
  -> SparseMatrix n n a -- ^ the square matrix.
  -> Maybe (SparseMatrix n1 m1 a)
{-# INLINEABLE squareSubset #-}
squareSubset rrxs mat = case unrefine rrxs of
  [] -> Nothing
  idxs -> Just $
    let 
      x' :: [SparseMatrix 1 n a] 
      x' = fmap (flip getRow mat) (fmap (_unsafeRefine . unrefine) idxs)
      x'' :: SparseMatrix n1 n a
      x'' = fromRows (_unsafeRefine x')
      x''' :: [SparseMatrix n1 1 a]
      x''' = (\m -> fmap (flip getCol m) (fmap (_unsafeRefine . unrefine) idxs)) x''
      x'''' :: SparseMatrix n1 m1 a
      x'''' = fromCols (_unsafeRefine x''')
    in x''''

-- | Convert a sparse matrix to the list of triplets (row, col, val). Compressed elements will not be included.
--   The returned vector is wrapped in a refinement.
toVector' :: Elem a => SparseMatrix n m a -> Refined (StorableVectorSizeEqualTo (n * m)) (VS.Vector (CTriplet a))
{-# INLINE toVector' #-}
toVector' m@(SparseMatrix fp) = _unsafeRefine . Internal.performIO $ do
  let !size = nonZeros m
  tris <- VSM.new size
  withForeignPtr fp $ \p ->
    VSM.unsafeWith tris $ \q ->
      Internal.call $ Internal.sparse_toList p q (toC size)
  VS.unsafeFreeze tris

-- | Convert a sparse matrix to the list of triplets (row, col, val). Compressed elements will not be included.
toVector :: Elem a => SparseMatrix n m a -> VS.Vector (CTriplet a)
{-# INLINE toVector #-}
toVector m@(SparseMatrix fp) = Internal.performIO $ do
  let !size = nonZeros m
  tris <- VSM.new size
  withForeignPtr fp $ \p ->
    VSM.unsafeWith tris $ \q ->
      Internal.call $ Internal.sparse_toList p q (toC size)
  VS.unsafeFreeze tris

-- | Convert a sparse matrix to the list of triplets (row, col, val). Compressed elements will not be included.
--   The returned list is wrapped in a refinement.
toList' :: Elem a => SparseMatrix n m a -> Refined (SizeEqualTo (n * m)) [(Int, Int, a)]
{-# INLINE toList' #-}
toList' = _unsafeRefine . Prelude.map fromC . VS.toList . toVector

-- | Convert a sparse matrix to the list of triplets (row, col, val). Compressed elements will not be included.
toList :: Elem a => SparseMatrix n m a -> [(Int, Int, a)]
{-# INLINE toList #-}
toList = Prelude.map fromC . VS.toList . toVector

-- | Construct a sparse matrix from a list of triples (row, val, col)
--
fromList :: (Elem a, KnownNat n, KnownNat m)
  => Refined (SizeEqualTo (n * m)) [(Int, Int, a)]
  -> SparseMatrix n m a
{-# INLINE fromList #-}
fromList = fromVector . _unsafeRefine . VS.fromList . fmap toC . unrefine

fromListSized :: (Elem a, KnownNat n, KnownNat m)
  => Refined (EqualTo n) Int
  -> Refined (EqualTo m) Int
  -> Refined (SizeEqualTo (n * m)) [(Int, Int, a)]
  -> SparseMatrix n m a
{-# INLINE fromListSized #-}
fromListSized _rows _cols = fromVectorSized _rows _cols . _unsafeRefine . VS.fromList . Prelude.map Internal.toC . unrefine

-- | Construct a sparse matrix of the given size from the storable vector of triplets (row, col, val)
fromVector :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => Refined (StorableVectorSizeEqualTo (n * m)) (VS.Vector (CTriplet a))
  -> SparseMatrix n m a
{-# INLINE fromVector #-}
fromVector = fromVectorSized (_unsafeRefine (natToInt @n)) (_unsafeRefine (natToInt @m))

fromVectorSized :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => Refined (EqualTo n) Int
  -> Refined (EqualTo m) Int
  -> Refined (StorableVectorSizeEqualTo (n * m)) (VS.Vector (CTriplet a))
  -> SparseMatrix n m a
{-# INLINE fromVectorSized #-}
fromVectorSized _rows _cols _tris =
  let !c_rs = toC $! unrefine _rows
      !c_cs = toC $! unrefine _cols
      !tris = unrefine _tris
      !len  = toC $! VS.length tris
  in Internal.performIO $ VS.unsafeWith tris $ \p ->
       alloca $ \pq -> do
         Internal.call $ Internal.sparse_fromList c_rs c_cs p len pq
         peek pq >>= _mk

-- | Convert a sparse matrix to a (n X m) dense list of values.
--   The returned value is wrapped in a refinement.
toDenseList' :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> Refined (SizeEqualTo n) [Refined (SizeEqualTo m) [a]]
{-# INLINE toDenseList' #-}
toDenseList' mat =  _unsafeRefine [ _unsafeRefine [_unsafeCoeff row col mat | col <- [0 .. cols mat - 1]] | row <- [0 .. rows mat - 1]]

-- | Convert a sparse matrix to a (n X m) dense list of values.
toDenseList :: forall n m a. (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> [[a]]
{-# INLINE toDenseList #-}
toDenseList mat = [[_unsafeCoeff row col mat | col <- [0 .. cols mat - 1]] | row <- [0 .. rows mat - 1]]

-- | Construct a sparsematrix from a two-dimensional list of values.
--   Zero values will be compressed.
fromDenseList :: forall n m a. (Elem a, Eq a, KnownNat n, KnownNat m)
  => Refined (SizeEqualTo n) [Refined (SizeEqualTo m) [a]]
  -> SparseMatrix n m a
{-# INLINEABLE fromDenseList #-}
fromDenseList _list =
  let _rows = natToInt @n
      _cols = natToInt @m
      list' :: [Refined (SizeEqualTo m) [a]]
      list' = unrefine _list
      list :: [[a]]
      list  = fmap unrefine list'
  in fromList $ _unsafeRefine $ do
    (row, vals) <- zip [0..] list
    (col, val)  <- zip [0..] vals
    guard $ val /= 0
    return (row, col, val)

-- | Construct a dense matrix from a sparse matrix
toMatrix :: (Elem a, KnownNat n, KnownNat m) => SparseMatrix n m a -> M.Matrix n m a
{-# INLINE toMatrix #-}
toMatrix (SparseMatrix fp) = Internal.performIO $ do
  m0 :: MM.IOMatrix n m a <- MM.new
  MM.unsafeWith m0 $ \_vals _rows _cols ->
    withForeignPtr fp $ \pm1 ->
      Internal.call $ Internal.sparse_toMatrix pm1 _vals _rows _cols
  M.unsafeFreeze m0

-- | Construct a sparse matrix from a dense matrix. zero-elements will be compressed.
fromMatrix :: (Elem a, KnownNat n, KnownNat m) => M.Matrix n m a -> SparseMatrix n m a
{-# INLINE fromMatrix #-}
fromMatrix m1 = Internal.performIO $ alloca $ \pm0 ->
  M.unsafeWith m1 $ \_vals _rows _cols -> do
    Internal.call $ Internal.sparse_fromMatrix _vals _rows _cols pm0
    peek pm0 >>= _mk

-- | Yield an immutable copy of the mutable matrix
freeze :: (Elem a, PrimMonad p) => SMM.MSparseMatrix n m (PrimState p) a -> p (SparseMatrix n m a)
{-# INLINE freeze #-}
freeze (SMM.MSparseMatrix fp) = SparseMatrix <$> _clone fp

-- | Yield a mutable copy of the immutable matrix.
thaw :: (Elem a, PrimMonad p) => SparseMatrix n m a -> p (SMM.MSparseMatrix n m (PrimState p) a)
{-# INLINE thaw #-}
thaw (SparseMatrix fp) = SMM.MSparseMatrix <$> _clone fp

-- | Unsafely convert a mutable matrix to an immutable one without copying. The mutable matrix may not be used after this operation.
unsafeFreeze :: (Elem a, PrimMonad p) => SMM.MSparseMatrix n m (PrimState p) a -> p (SparseMatrix n m a) 
{-# INLINE unsafeFreeze #-}
unsafeFreeze (SMM.MSparseMatrix fp) = return $! SparseMatrix fp

-- | Unsafely convert an immutable matrix to a mutable one without copying. The immutable matrix may not be used after this operation.
unsafeThaw :: (Elem a, PrimMonad p) => SparseMatrix n m a -> p (SMM.MSparseMatrix n m (PrimState p) a) 
{-# INLINE unsafeThaw #-}
unsafeThaw (SparseMatrix fp) = return $! SMM.MSparseMatrix fp

-- | Return a single row of the sparse matrix.
getRow :: forall n m a. (Elem a, KnownNat n, KnownNat m, 1 <= n)
  => Refined (FromTo 0 n) Int
  -> SparseMatrix n m a
  -> SparseMatrix 1 m a
{-# INLINE getRow #-}
getRow row mat = block row (_unsafeRefine 0) (Proxy @1) (Proxy @m) mat

-- | Return a single column of the sparse matrix.
getCol :: forall n m a. (Elem a, KnownNat n, KnownNat m, 1 <= m)
  => Refined (FromTo 0 m) Int
  -> SparseMatrix n m a
  -> SparseMatrix n 1 a
{-# INLINE getCol #-}
getCol col mat = block (_unsafeRefine 0) col (Proxy @n) (Proxy @1) mat

-- | Return all rows of the sparse matrix.
getRows :: forall n m a. (Elem a, KnownNat n, KnownNat m, 1 <= n) => SparseMatrix n m a -> [SparseMatrix 1 m a]
{-# INLINE [1] getRows #-}
getRows mat = fmap (\i -> getRow (_unsafeRefine i) mat) [0 .. (natToInt @n) - 1]

getRows' :: forall n m a. (Elem a, KnownNat n, KnownNat m, 1 <= n) => SparseMatrix n m a -> Refined (SizeEqualTo n) [SparseMatrix 1 m a]
{-# INLINE getRows' #-}
getRows' = _unsafeRefine . getRows

-- | Return all columns of a sparse matrix.
getCols :: forall n m a. (Elem a, KnownNat n, KnownNat m, 1 <= m) => SparseMatrix n m a -> [SparseMatrix n 1 a]
{-# INLINE [1] getCols #-}
getCols mat = fmap (\i -> getCol (_unsafeRefine i) mat) [0 .. (natToInt @m) - 1]

getCols' :: forall n m a. (Elem a, KnownNat n, KnownNat m, 1 <= m) => SparseMatrix n m a -> Refined (SizeEqualTo m) [SparseMatrix n 1 a]
{-# INLINE getCols' #-}
getCols' = _unsafeRefine . getCols

-- | Get a sparse matrix from a list of columns.
fromCols :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => Refined (SizeEqualTo m) [SparseMatrix n 1 a]
  -> SparseMatrix n m a
{-# INLINE fromCols #-}
fromCols _cols =
  let cols_ = _unsafeRefine $ concatMap toList $ unrefine _cols
      n = _unsafeRefine (natToInt @n)
      m = _unsafeRefine (natToInt @m)
  in fromListSized n m cols_

-- | Get a sparse matrix from a list of rows.
fromRows :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => Refined (SizeEqualTo n) [SparseMatrix 1 m a]
  -> SparseMatrix n m a
{-# INLINE fromRows #-}
fromRows _rows =
  let rows_ = _unsafeRefine $ concatMap toList $ unrefine _rows
      n = _unsafeRefine (natToInt @n)
      m = _unsafeRefine (natToInt @m)
  in fromListSized n m rows_

_unop :: Storable b => (CSparseMatrixPtr a -> Ptr b -> IO CString) -> (b -> IO c) -> SparseMatrix n m a -> c
{-# INLINEABLE _unop #-}
_unop f g (SparseMatrix fp) = Internal.performIO $
  withForeignPtr fp $ \p ->
    alloca $ \pq -> do
      Internal.call (f p pq)
      peek pq >>= g

_binop :: Storable b => (CSparseMatrixPtr a -> CSparseMatrixPtr a -> Ptr b -> IO CString) -> (b -> IO c) -> SparseMatrix n m a -> SparseMatrix n1 m1 a -> c
{-# INLINEABLE _binop #-}
_binop f g (SparseMatrix fp1) (SparseMatrix fp2) = Internal.performIO $
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
      alloca $ \pq -> do
        Internal.call (f p1 p2 pq)
        peek pq >>= g

_getvec :: (Elem a, Storable b) => (Ptr (CSparseMatrix a) -> Ptr CInt -> Ptr (Ptr b) -> IO CString) -> SparseMatrix n m a -> VS.Vector b
{-# INLINEABLE _getvec #-}
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
{-# INLINEABLE _clone #-}
_clone fp = unsafePrimToPrim $ withForeignPtr fp $ \p -> alloca $ \pq -> do
  Internal.call $ Internal.sparse_clone p pq
  q <- peek pq
  FC.newForeignPtr q $ Internal.call $ Internal.sparse_free q

_mk :: Elem a => Ptr (CSparseMatrix a) -> IO (SparseMatrix n m a)
{-# INLINEABLE _mk #-}
_mk p = SparseMatrix <$> FC.newForeignPtr p (Internal.call $ Internal.sparse_free p)

_unsafeCoeff :: (Elem a, KnownNat n) => Int -> Int -> SparseMatrix n m a -> a
{-# INLINE _unsafeCoeff #-}
_unsafeCoeff !row !col (SparseMatrix fp) =
  let !c_row = toC row
      !c_col = toC col
  in Internal.performIO $ withForeignPtr fp $ \p -> alloca $ \pq -> do
       Internal.call $ Internal.sparse_coeff p c_row c_col pq
       fromC <$> peek pq
