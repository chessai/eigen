{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoStarIsType #-}
#endif
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Eigen.Matrix
  ( 
    -- * Types 
    Matrix(..)
  , Vec(..)
  , MatrixXf
  , MatrixXd
  , MatrixXcf
  , MatrixXcd

    -- * Common API
  , Elem
  , C
  , natToInt

    -- * Querying a Matrix
  , null
  , square
  , rows
  , cols
  , rows'
  , cols'
  , dims
    
    -- * Constructing a Matrix
  , empty
  , constant
  , zero
  , ones
  , identity
  , random
  , diagonal

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
  , toList'
  ) where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (ST)
import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Constraint.Nat
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import GHC.TypeLits (Nat, type (*), type (<=), KnownNat)
import Prelude hiding (map, null, filter, length, foldl, any, all, sum)
import Refined
import qualified Data.List as List
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Eigen.Internal
  ( Elem
  , Cast(..)
  , natToInt
  , _unsafeRefine
  )
import qualified Eigen.Internal as Internal
import qualified Eigen.Matrix.Mutable as M

-- | Matrix to be used in pure computations.
--
--   * Uses column majour memory layout.
--
--   * Has a copy-free FFI using the <http://eigen.tuxfamily.org Eigen> library.
--
newtype Matrix :: Nat -> Nat -> Type -> Type where
  Matrix :: Vec (n * m) a -> Matrix n m a

-- | Used internally to track the size and corresponding C type of the matrix.
newtype Vec :: Nat -> Type -> Type where
  Vec :: VS.Vector (C a) -> Vec n a

instance forall n m a. (Elem a, Show a, KnownNat n, KnownNat m) => Show (Matrix n m a) where
  show m = List.concat
    [ "Matrix ", show (rows m), "x", show (cols m)
    , "\n", List.intercalate "\n" $ List.map (List.intercalate "\t" . List.map show) $ toList m, "\n"
    ]

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
empty = _unsafeMatrix (VS.empty)

-- | Is matrix empty?
--
--   Note that this function is redundant given the type information
--   that makes up a 'Matrix'.
null :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Bool
{-# INLINE null #-}
null m = cols m == 0 && rows m == 0

-- | Is matrix square?
--
--   Note that this function is redundant given the type information that
--   makes up a 'Matrix'.
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
{-# INLINE identity #-}
identity =
  Internal.performIO $ do
     m :: M.IOMatrix n m a <- M.new
     Internal.call $ M.unsafeWith m Internal.identity
     unsafeFreeze m

-- | The random matrix of a given size
random :: forall n m a. (Elem a, KnownNat n, KnownNat m) => Matrix n m a
random = do
  Internal.performIO $ do
    m :: M.IOMatrix n m a <- M.new
    Internal.call $ M.unsafeWith m Internal.random
    unsafeFreeze m

withDims :: forall n m a. (Elem a, KnownNat n, KnownNat m) => (Int -> Int -> VS.Vector (C a)) -> Matrix n m a
{-# INLINE withDims #-}
withDims f =
  let !r = natToInt @n
      !c = natToInt @m
  in _unsafeMatrix $ f r c

-- | The number of rows in the matrix, wrapped in a refinement.
rows' :: forall n m a. KnownNat n => Matrix n m a -> Refined (EqualTo n) Int
{-# INLINE rows' #-}
rows' = _unsafeRefine . rows

-- | The number of colums in the matrix
cols' :: forall n m a. KnownNat m => Matrix n m a -> Refined (EqualTo m) Int
{-# INLINE cols' #-}
cols' = _unsafeRefine . cols

-- | The number of rows in the matrix, wrapped in a refinement.
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

-- | Matrix coefficient at the given row and column
(!) :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => Matrix n m a
  -> (Refined (FromTo 0 n) Int, Refined (FromTo 0 m) Int)
  -> a
{-# INLINE (!) #-}
(!) m (row,col) = coeff row col m

-- | Return the value at the given position.
coeff :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => Refined (FromTo 0 n) Int
  -> Refined (FromTo 0 m) Int
  -> Matrix n m a
  -> a
{-# INLINE coeff #-}
coeff r c m@(_unwrapMatrix -> vals) =
  let !row  = unrefine r
      !col  = unrefine c
  in fromC $! VS.unsafeIndex vals $! col * rows m + row

-- | Given a generation function `f :: Int -> Int -> a`, construct a Matrix of known size
--   using points in the matrix as inputs.
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

-- | Given a predicate p, determine if all values in the Matrix satisfy p.
all :: (Elem a, KnownNat n, KnownNat m) => (a -> Bool) -> Matrix n m a -> Bool
all f (_unwrapMatrix -> vals) = VS.all (f . fromC) vals

-- | Given a predicate p, determine if any values in the Matrix satisfy p.
any :: (Elem a, KnownNat n, KnownNat m) => (a -> Bool) -> Matrix n m a -> Bool
any f (_unwrapMatrix -> vals) = VS.any (f . fromC) vals

-- | Given a predicate p, determine how many values in the Matrix satisfy p.
count :: (Elem a, KnownNat n, KnownNat m) => (a -> Bool) -> Matrix n m a -> Int
count f (_unwrapMatrix -> vals) = VS.foldl' (\n x-> if f (fromC x) then (n + 1) else n) 0 vals

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

-- | Add two matrices.
add :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix n m a -> Matrix n m a
add m1 m2 = _binop Internal.add m1 m2

-- | Subtract two matrices.
sub :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Matrix n m a -> Matrix n m a
sub m1 m2 = _binop Internal.sub m1 m2

-- | Multiply two matrices.
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
map f (_unwrapMatrix -> vals) = _unsafeMatrix $ VS.map (toC . f . fromC) vals

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
imap f (_unwrapMatrix -> vals) =
  withDims $ \rs _ ->
    VS.imap (\n ->
      let (c,r) = divMod n rs
      in toC . f r c . fromC) vals

-- | Provide a view of the matrix for extraction of a subset.
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

-- | Filter elements in the matrix. Filtered elements will be replaced by 0.
filter :: Elem a => (a -> Bool) -> Matrix n m a -> Matrix n m a
filter f = map (\x -> if f x then x else 0)

-- | Filter elements in the matrix with an indexed predicate. Filtered elements will be replaces by 0.
ifilter :: (Elem a, KnownNat n, KnownNat m) => (Int -> Int -> a -> Bool) -> Matrix n m a -> Matrix n m a
ifilter f = imap (\r c x -> if f r c x then x else 0)

-- | The length of the matrix.
length :: forall n m a. (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Int
length _ = natToInt @n * natToInt @m

-- | Left fold of a matrix, where accumulation is lazy.
foldl :: (Elem a, KnownNat n, KnownNat m) => (b -> a -> b) -> b -> Matrix n m a -> b
foldl f b (_unwrapMatrix -> vals) = VS.foldl (\a x -> f a (fromC x)) b vals

-- | Right fold of a matrix, where accumulation is strict.
foldl' :: Elem a => (b -> a -> b) -> b -> Matrix n m a -> b
foldl' f b (_unwrapMatrix -> vals) = VS.foldl' (\ !a x -> f a (fromC x)) b vals

-- | Return the diagonal of a matrix.
diagonal :: (Elem a, KnownNat n, KnownNat m, r ~ Min n m, KnownNat r) => Matrix n m a -> Matrix r 1 a
diagonal = _unop Internal.diagonal

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
normalize (_unwrapMatrix -> vals) = Internal.performIO $ do
  vals' <- VS.thaw vals
  VSM.unsafeWith vals' $ \p ->
    let !rs = natToInt @n
        !cs = natToInt @m
    in Internal.call $ Internal.normalize p (toC rs) (toC cs)
  _unsafeMatrix <$> VS.unsafeFreeze vals'

-- | Apply a destructive operation to a matrix. The operation will be performed in-place, if it is safe
--   to do so - otherwise, it will create a copy of the matrix.
modify :: (Elem a, KnownNat n, KnownNat m) => (forall s. M.MMatrix n m s a -> ST s ()) -> Matrix n m a -> Matrix n m a
modify f (_unwrapMatrix -> vals) = _unsafeMatrix $ VS.modify (f . M.fromVector ) vals

block :: forall n m a br bc. (Elem a, KnownNat n, KnownNat m, KnownNat br, KnownNat bc, br <= n, bc <= m)
  => Refined (FromTo 0 n) Int -- ^ starting row
  -> Refined (FromTo 0 m) Int -- ^ starting col
  -> Proxy br                 -- ^ block of rows
  -> Proxy bc                 -- ^ block of cols
  -> Matrix n m a       -- ^ extract from this
  -> Matrix br bc a     -- ^ extraction
{-# INLINE block #-}
block sr sc _ _ m =
  let !c_startRow = unrefine sr
      !c_startCol = unrefine sc
  in generate $ \row col -> coeff (_unsafeRefine $ c_startRow + row) (_unsafeRefine $ c_startCol + col) m
  
-- | Turn a mutable matrix into an immutable matrix without copying.
--   The mutable matrix should not be modified after this conversion.
unsafeFreeze :: (Elem a, KnownNat n, KnownNat m, PrimMonad p) => M.MMatrix n m (PrimState p) a -> p (Matrix n m a)
unsafeFreeze m = VS.unsafeFreeze (M.vals m) >>= pure . _unsafeMatrix
  
-- | Pass a pointer to the matrix's data to the IO action. The data may not be modified through the pointer.
unsafeWith  :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> (Ptr (C a) -> CInt -> CInt -> IO b) -> IO b
unsafeWith m@(_unwrapMatrix -> vals) f =
  VS.unsafeWith vals $ \p ->
    let !rs = toC $! rows m
        !cs = toC $! cols m
    in f p rs cs

-- | Convert a matrix to a list.
toList :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> [[a]]
{-# INLINE toList #-}
toList m@(_unwrapMatrix -> vals)
  | null m = []
  | otherwise = [[fromC $ vals `VS.unsafeIndex` (col * _rows + row) | col <- [0..pred _cols]] | row <- [0..pred _rows]]
  where
    !_rows = rows m
    !_cols = cols m

-- | Convert a matrix to a list, wrapped in a refinement.
toList' :: (Elem a, KnownNat n, KnownNat m) => Matrix n m a -> Refined (SizeEqualTo n) [Refined (SizeEqualTo m) [a]]
toList' = _unsafeRefine . fmap _unsafeRefine . toList

fromList :: forall n m a. (Elem a, KnownNat n, KnownNat m)
  => Refined (SizeEqualTo n) [Refined (SizeEqualTo m) [a]]
  -> Matrix n m a
fromList (((fmap unrefine) . unrefine) -> list) =
  let _rows = natToInt @n
      _cols = natToInt @m
  in _unsafeMatrix $ VS.create $ do
      vm <- VSM.replicate (_rows * _cols) (toC (0 :: a))
      forM_ (zip [0..] list) $ \(row,vals) ->
        forM_ (zip [0..] vals) $ \(col,val) ->
          VSM.write vm (col * _rows + row) (toC val)
      pure vm

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

_unsafeMatrix :: VS.Vector (C a) -> Matrix n m a
{-# INLINE _unsafeMatrix #-}
_unsafeMatrix = coerce

_unwrapMatrix :: Matrix n m a -> VS.Vector (C a)
{-# INLINE _unwrapMatrix #-}
_unwrapMatrix = coerce
