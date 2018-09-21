{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Eigen.Matrix
  ( Elem
  , C
  , natToInt
  , Row(..)
  , Col(..)
  
  , Matrix(..)
  , Vec(..)
  , MatrixXf
  , MatrixXd
  , MatrixXcf
  , MatrixXcd

  , encode
  , decode
  
  , empty
  , null
  , square

  , constant
  , zero
  , ones
  , identity
  , random
  , rows
  , cols

  , dims
  , (!)
  , coeff
  , generate
  , sum
  , prod
  , mean
  , trace
  , all
  , any
  , count
  , norm
  , squaredNorm
  , blueNorm
  , hypotNorm
  , determinant
  , add
  , sub
  , mul
  , map
  , imap
  , TriangularMode(..)
  , triangularView
  , filter
  , ifilter
  , length
  , foldl
  , foldl'
  , inverse
  , adjoint
  , transpose
  , conjugate
  , normalize
  , modify
  , block
  , unsafeFreeze
  , unsafeWith
  , fromList
  , toList


  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Prelude hiding
  (map, null, filter, length, foldl, any, all, sum)
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Binary (Binary(..))
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BSL
import Data.Complex (Complex)
-- import Data.Constraint.Nat
import Data.Eigen.Internal
  ( Elem
  , C(..)
  , natToInt
  , Row(..)
  , Col(..)
  )
import qualified Data.Eigen.Internal as Internal
import qualified Data.Eigen.Matrix.Mutable as M
import qualified Data.List as List
import Data.Kind (Type)
import GHC.TypeLits (Nat, type (*), type (<=), KnownNat)
import Foreign.C.Types (CInt)
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

newtype Matrix :: Nat -> Nat -> Type -> Type where
  Matrix :: Vec (n * m) a -> Matrix n m a

newtype Vec :: Nat -> Type -> Type where
  Vec :: VS.Vector (C a) -> Vec n a

instance forall n m a. (Elem a, Show a, KnownNat n, KnownNat m) => Show (Matrix n m a) where
  show m = List.concat
    [ "Matrix ", show (rows m), "x", show (cols m)
    , "\n", List.intercalate "\n" $ List.map (List.intercalate "\t" . List.map show) $ toList m, "\n"
    ]

instance forall n m a. (KnownNat n, KnownNat m, Elem a) => Binary (Matrix n m a) where
  put (Matrix (Vec vals)) = do
    put $ Internal.magicCode (undefined :: C a)
    put $ natToInt @n
    put $ natToInt @m
    put vals

  get = do
    get >>= (`when` fail "wrong matrix type") . (/= Internal.magicCode (undefined :: C a))
    Matrix . Vec <$> get

-- | Encode the sparse matrix as a lazy bytestring
encode :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> BSL.ByteString
encode = Binary.encode

-- | Decode the sparse matrix from a lazy bytestring
decode :: (Elem a, KnownNat n, KnownNat m) => BSL.ByteString -> Matrix n m a
decode = Binary.decode

-- | Alias for single precision matrix
type MatrixXf n m = Matrix n m Float
-- | Alias for double precision matrix
type MatrixXd n m = Matrix n m Double
-- | Alias for single precision matrix of complex numbers
type MatrixXcf n m = Matrix n m (Complex Float)
-- | Alias for double precision matrix of complex numbers
type MatrixXcd n m = Matrix n m (Complex Double)

-- | Construct an empty 0x0 matrix
empty :: Elem a => Matrix 0 0 a
{-# INLINE empty #-}
empty = Matrix (Vec (VS.empty))

-- | Is matrix empty?
null :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Bool
{-# INLINE null #-}
null m = cols m == 0 && rows m == 0

-- | Is matrix square?
--
square :: forall n m a. (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Bool
{-# INLINE square #-}
square _ = natToInt @n == natToInt @m

-- | Matrix where all coeffs are filled with the given value
constant :: forall n m a. (Elem a, KnownNat n, KnownNat m) => a -> Matrix n m a
{-# INLINE constant #-}
constant !val =
  let !cval = toC val
  in withDims $ \rs cs -> VS.replicate (rs * cs) cval

-- | Matrix where all coeffs are filled with 0
zero :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a
{-# INLINE zero #-}
zero = constant 0

-- | Matrix where all coeffs are filled with 1
ones :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a
{-# INLINE ones #-}
ones = constant 1

-- | The identity matrix (not necessarily square)
identity :: forall n m a. (Elem a, KnownNat n, KnownNat m) => Matrix n m a
identity =
  Internal.performIO $ do
     m :: M.IOMatrix n m a <- M.new
     Internal.call $ M.unsafeWith m Internal.identity
     unsafeFreeze m

-- | The random matrix of a given size
random :: forall n m a. (Elem a, KnownNat n, KnownNat m) => IO (Matrix n m a)
random = do
  m :: M.IOMatrix n m a <- M.new
  Internal.call $ M.unsafeWith m Internal.random
  unsafeFreeze m

withDims :: forall n m a. (Elem a, KnownNat n, KnownNat m) => (Int -> Int -> VS.Vector (C a)) -> Matrix n m a
{-# INLINE withDims #-}
withDims f =
  let !r = natToInt @n
      !c = natToInt @m
  in Matrix $ Vec $ f r c

-- | The number of rows in the matrix
rows :: forall n m a. KnownNat n => Matrix n m a -> Int
{-# INLINE rows #-}
rows _ = natToInt @n

-- | The number of colums in the matrix
cols :: forall n m a. KnownNat m => Matrix n m a -> Int
{-# INLINE cols #-}
cols _ = natToInt @m

-- | Return Matrix size as a pair of (rows, cols)
dims :: forall n m a. (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> (Int, Int)
{-# INLINE dims #-}
dims _ = (natToInt @n, natToInt @m)

(!) :: forall n m a r c. (Elem a, KnownNat n, KnownNat r, KnownNat c, r <= n, c <= m) => Row r -> Col c -> Matrix n m a -> a
{-# INLINE (!) #-}
(!) = coeff

coeff :: forall n m a r c. (Elem a, KnownNat n, KnownNat r, KnownNat c, r <= n, c <= m) => Row r -> Col c -> Matrix n m a -> a
{-# INLINE coeff #-}
coeff _ _ m@(Matrix (Vec vals)) =
  let !row  = natToInt @r
      !col  = natToInt @c
  in fromC $! VS.unsafeIndex vals $! col * rows m + row

unsafeCoeff :: (Elem a, KnownNat n) => Int -> Int -> Matrix n m a -> a
{-# INLINE unsafeCoeff #-}
unsafeCoeff row col m@(Matrix (Vec vals)) = fromC $! VS.unsafeIndex vals $! col * rows m + row

generate :: forall n m a. (Elem a, KnownNat n, KnownNat m) => (Int -> Int -> a) -> Matrix n m a
generate f = withDims $ \rs cs -> VS.create $ do
  vals :: VSM.MVector s (C a) <- VSM.new (rs * cs)
  forM_ [0 .. pred rs] $ \r ->
    forM_ [0 .. pred cs] $ \c ->
      VSM.write vals (c * rs + r) (toC $! f r c)
  pure vals

-- | The sum of all coefficients in the matrix
sum :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> a
sum = _prop Internal.sum

-- | The product of all coefficients in the matrix
prod :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> a
prod = _prop Internal.prod

-- | The arithmetic mean of all coefficients in the matrix
mean :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> a
mean = _prop Internal.mean

-- | The trace of a matrix is the sum of the diagonal coefficients.
--   
--   'trace' m == 'sum' ('diagonal' m)
trace :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> a
trace = _prop Internal.trace

all :: (Elem a, KnownNat n, KnownNat m) => (a -> Bool) -> Matrix n m a -> Bool
all f (Matrix (Vec vals)) = VS.all (f . fromC) vals

any :: (Elem a, KnownNat n, KnownNat m) => (a -> Bool) -> Matrix n m a -> Bool
any f (Matrix (Vec vals)) = VS.any (f . fromC) vals

count :: (Elem a, KnownNat n, KnownNat m) => (a -> Bool) -> Matrix n m a -> Int
count f (Matrix (Vec vals)) = VS.foldl' (\n x-> if f (fromC x) then (n + 1) else n) 0 vals

norm, squaredNorm, blueNorm, hypotNorm :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> a

{-| For vectors, the l2 norm, and for matrices the Frobenius norm.
    In both cases, it consists in the square root of the sum of the square of all the matrix entries.
    For vectors, this is also equals to the square root of the dot product of this with itself.
-}
norm = _prop Internal.norm

-- | For vectors, the squared l2 norm, and for matrices the Frobenius norm. In both cases, it consists in the sum of the square of all the matrix entries. For vectors, this is also equals to the dot product of this with itself.
squaredNorm = _prop Internal.squaredNorm

-- | The l2 norm of the matrix using the Blue's algorithm. A Portable Fortran Program to Find the Euclidean Norm of a Vector, ACM TOMS, Vol 4, Issue 1, 1978.
blueNorm = _prop Internal.blueNorm

-- | The l2 norm of the matrix avoiding undeflow and overflow. This version use a concatenation of hypot calls, and it is very slow.
hypotNorm = _prop Internal.hypotNorm

-- | The determinant of the matrix
determinant :: forall n a. (Elem a, KnownNat n) => Matrix n n a -> a
determinant m = _prop Internal.determinant m

add :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix n m a -> Matrix n m a
add m1 m2 = _binop Internal.add m1 m2

sub :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix n m a -> Matrix n m a
sub m1 m2 = _binop Internal.sub m1 m2

mul :: (Elem a, KnownNat p, KnownNat q, KnownNat r) => Matrix p q a -> Matrix q r a -> Matrix p r a
mul m1 m2 = _binop Internal.mul m1 m2

{- | Apply a given function to each element of the matrix.
Here is an example how to implement scalar matrix multiplication:
>>> let a = fromList [[1,2],[3,4]] :: MatrixXf 2 2
>>> a
Matrix 2x2
1.0 2.0
3.0 4.0
>>> map (*10) a
Matrix 2x2
10.0    20.0
30.0    40.0
-}
map :: Elem a => (a -> a) -> Matrix n m a -> Matrix n m a
map f (Matrix (Vec vals)) = Matrix $ Vec $ VS.map (toC . f . fromC) vals

{- | Apply a given function to each element of the matrix.
Here is an example how upper triangular matrix can be implemented:
>>> let a = fromList [[1,2,3],[4,5,6],[7,8,9]] :: MatrixXf
>>> a
Matrix 3x3
1.0 2.0 3.0
4.0 5.0 6.0
7.0 8.0 9.0
>>> imap (\row col val -> if row <= col then val else 0) a
Matrix 3x3
1.0 2.0 3.0
0.0 5.0 6.0
0.0 0.0 9.0
-}
imap :: (Elem a, KnownNat n, KnownNat m) => (Int -> Int -> a -> a) -> Matrix n m a -> Matrix n m a
imap f (Matrix (Vec vals)) =
  withDims $ \rs _ ->
    VS.imap (\n ->
      let (c,r) = divMod n rs
      in toC . f r c . fromC) vals

data TriangularMode
  -- | View matrix as a lower triangular matrix.
  = Lower
  -- | View matrix as an upper triangular matrix.
  | Upper
  -- | View matrix as a lower triangular matrix with zeros on the diagonal.
  | StrictlyLower
  -- | View matrix as an upper triangular matrix with zeros on the diagonal.
  | StrictlyUpper
  -- | View matrix as a lower triangular matrix with ones on the diagonal.
  | UnitLower
  -- | View matrix as an upper triangular matrix with ones on the diagonal.
  | UnitUpper
  deriving (Eq, Enum, Show, Read)

-- | Triangular view extracted from the current matrix
triangularView :: (Elem a, KnownNat n, KnownNat m) => TriangularMode -> Matrix n m a -> Matrix n m a
triangularView = \case
  Lower         -> imap $ \row col val -> case compare row col of { LT -> 0; _ -> val }
  Upper         -> imap $ \row col val -> case compare row col of { GT -> 0; _ -> val }
  StrictlyLower -> imap $ \row col val -> case compare row col of { GT -> val; _ -> 0 }
  StrictlyUpper -> imap $ \row col val -> case compare row col of { LT -> val; _ -> 0 }
  UnitLower     -> imap $ \row col val -> case compare row col of { GT -> val; LT -> 0; EQ -> 1 }
  UnitUpper     -> imap $ \row col val -> case compare row col of { LT -> val; GT -> 0; EQ -> 1 }

-- | Filter elements in the matrix. Filtered elements will be replaced by 0
filter :: Elem a => (a -> Bool) -> Matrix n m a -> Matrix n m a
filter f = map (\x -> if f x then x else 0)

ifilter :: (Elem a, KnownNat n, KnownNat m) => (Int -> Int -> a -> Bool) -> Matrix n m a -> Matrix n m a
ifilter f = imap (\r c x -> if f r c x then x else 0)

length :: forall n m a r. (Elem a, KnownNat n, KnownNat m, r ~ (n * m), KnownNat r) => Matrix n m a -> Int
length _ = natToInt @r

foldl :: (Elem a, KnownNat n, KnownNat m) => (b -> a -> b) -> b -> Matrix n m a -> b
foldl f b (Matrix (Vec vals)) = VS.foldl (\a x -> f a (fromC x)) b vals

foldl' :: Elem a => (b -> a -> b) -> b -> Matrix n m a -> b
foldl' f b (Matrix (Vec vals)) = VS.foldl' (\ !a x -> f a (fromC x)) b vals

--diagonal :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix (Min n m) 1 a
--diagonal = _unop Internal.diagonal

{- | Inverse of the matrix
For small fixed sizes up to 4x4, this method uses cofactors. In the general case, this method uses PartialPivLU decomposition
-}
inverse :: forall n a. (Elem a, KnownNat n) => Matrix n n a -> Matrix n n a
inverse = _unop Internal.inverse

-- | Adjoint of the matrix
adjoint :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix m n a
adjoint = _unop Internal.adjoint

-- | Transpose of the matrix
transpose :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix m n a
transpose = _unop Internal.transpose

-- | Conjugate of the matrix
conjugate :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix n m a
conjugate = _unop Internal.conjugate

-- | Normalise the matrix by dividing it on its 'norm'
normalize :: forall n m a. (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix n m a
normalize (Matrix (Vec vals)) = Internal.performIO $ do
  vals' <- VS.thaw vals
  VSM.unsafeWith vals' $ \p ->
    let !rs = natToInt @n
        !cs = natToInt @m
    in Internal.call $ Internal.normalize p (toC rs) (toC cs)
  Matrix . Vec <$> VS.unsafeFreeze vals'

-- | Apply a destructive operation to a matrix. The operation will be performed in-place, if it is safe
--   to do so - otherwise, it will create a copy of the matrix.
modify :: (Elem a, KnownNat n, KnownNat m) => (forall s. M.MMatrix n m s a -> ST s ()) -> Matrix n m a -> Matrix n m a
modify f (Matrix (Vec vals)) = Matrix $ Vec $ VS.modify (f . M.fromVector ) vals

-- | Extract rectangular block from matrix defined by startRow startCol blockRows blockCols
block :: forall sr sc br bc n m a.
     (Elem a, KnownNat sr, KnownNat sc, KnownNat br, KnownNat bc, KnownNat n, KnownNat m)
  => (sr <= n, sc <= m, br <= n, bc <= m)
  => Row sr -- ^ starting row
  -> Col sc -- ^ starting col
  -> Row br -- ^ block of rows
  -> Col bc -- ^ block of cols
  -> Matrix n m a -- ^ extract from this
  -> Matrix br bc a -- ^ extraction
block _ _ _ _ m =
  let !startRow = natToInt @sr
      !startCol = natToInt @sc
  in generate $ \row col -> unsafeCoeff (startRow + row) (startCol + col) m

unsafeFreeze :: (Elem a, KnownNat n, KnownNat m, PrimMonad p) => M.MMatrix n m (PrimState p) a -> p (Matrix n m a)
unsafeFreeze m = VS.unsafeFreeze (M.vals m) >>= pure . Matrix . Vec
  
-- | Pass a pointer to the matrix's data to the IO action. The data may not be modified through the pointer.
unsafeWith  :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> (Ptr (C a) -> CInt -> CInt -> IO b) -> IO b
unsafeWith m@(Matrix (Vec (vals))) f =
  VS.unsafeWith vals $ \p ->
    let !rs = toC $! rows m
        !cs = toC $! cols m
    in f p rs cs

_prop :: (Elem a, KnownNat n, KnownNat m) => (Ptr (C a) -> Ptr (C a) -> CInt -> CInt -> IO CString) -> Matrix n m a -> a
{-# INLINE _prop #-}
_prop f m = fromC $ Internal.performIO $ alloca $ \p -> do
   Internal.call $ unsafeWith m (f p)
   peek p

_binop :: forall n m n1 m1 n2 m2 a. (Elem a, KnownNat n, KnownNat m, KnownNat n1, KnownNat m1, KnownNat n2, KnownNat m2)
  => (Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString)
  -> Matrix n m a
  -> Matrix n1 m1 a
  -> Matrix n2 m2 a
{-# INLINE _binop #-}
_binop g m1 m2 = Internal.performIO $ do
  m0 :: M.IOMatrix n2 m2 a <- M.new
  M.unsafeWith m0 $ \vals0 rows0 cols0 ->
      unsafeWith m1 $ \vals1 rows1 cols1 ->
          unsafeWith m2 $ \vals2 rows2 cols2 ->
              Internal.call $ g
                vals0 rows0 cols0
                vals1 rows1 cols1
                vals2 rows2 cols2
  unsafeFreeze m0

_unop :: forall n m n1 m1 a. (Elem a, KnownNat n, KnownNat m, KnownNat n1, KnownNat m1)
  => (Ptr (C a) -> CInt -> CInt -> Ptr (C a) -> CInt -> CInt -> IO CString)
  -> Matrix n m a
  -> Matrix n1 m1 a
{-# INLINE _unop #-}
_unop g m1 = Internal.performIO $ do
  m0 :: M.IOMatrix n1 m1 a <- M.new
  M.unsafeWith m0 $ \vals0 rows0 cols0 ->
      unsafeWith m1 $ \vals1 rows1 cols1 ->
          Internal.call $ g
              vals0 rows0 cols0
              vals1 rows1 cols1
  unsafeFreeze m0

toList :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> [[a]]
{-# INLINE toList #-}
toList m@(Matrix (Vec vals))
  | null m = []
  | otherwise = [[fromC $ vals `VS.unsafeIndex` (col * _rows + row) | col <- [0..pred _cols]] | row <- [0..pred _rows]]
  where
    !_rows = rows m
    !_cols = cols m

fromList :: forall n m a. (Elem a, KnownNat n, KnownNat m) => [[a]] -> Maybe (Matrix n m a)
fromList list = mm
  where
    myRows = natToInt @n
    myCols = natToInt @m
    _rows = List.length list
    _cols = List.foldl' max 0 $ List.map List.length list
    mm = if ((myRows /= _rows) || (myCols /= _cols)) then Nothing else (Just . Matrix . Vec) $ VS.create $ do
      vm <- VSM.replicate (_rows * _cols) (toC (0 :: a))
      forM_ (zip [0..] list) $ \(row,vals) ->
        forM_ (zip [0..] vals) $ \(col, val) ->
          VSM.write vm (col * _rows + row) (toC val)
      return vm
