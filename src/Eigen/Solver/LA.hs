{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Eigen.Solver.LA
  ( Decomposition(..)
  , solve
  , relativeError
  , rank
  , kernel
  , image
  , linearRegression
  ) where

import Eigen.Internal (Elem, Cast(..))
import Eigen.Matrix
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import GHC.TypeLits (KnownNat)
import GHC.Types
import Prelude
import qualified Eigen.Internal as Internal
import qualified Eigen.Matrix.Mutable as MM
import qualified Eigen.Matrix as M
import qualified Data.List as List
import qualified Data.Vector.Storable as VS
import qualified Foreign.Concurrent as FC
--import qualified Prelude as Prelude

{- |
@
Decomposition           Requirements on the matrix          Speed   Accuracy  Rank  Kernel  Image

PartialPivLU            Invertible                          ++      +         -     -       -
FullPivLU               None                                -       +++       +     +       +
HouseholderQR           None                                ++      +         -     -       -
ColPivHouseholderQR     None                                +       ++        +     -       -
FullPivHouseholderQR    None                                -       +++       +     -       -
LLT                     Positive definite                   +++     +         -     -       -
LDLT                    Positive or negative semidefinite   +++     ++        -     -       -
JacobiSVD               None                                -       +++       +     -       -
@
The best way to do least squares solving for square matrices is with a SVD decomposition ('JacobiSVD')
-}
data Decomposition
  -- | LU decomposition of a matrix with partial pivoting.
  = PartialPivLU
  -- | LU decomposition of a matrix with complete pivoting.
  | FullPivLU
  -- | Householder QR decomposition of a matrix.
  | HouseholderQR
  -- | Householder rank-revealing QR decomposition of a matrix with column-pivoting.
  | ColPivHouseholderQR
  -- | Householder rank-revealing QR decomposition of a matrix with full pivoting.
  | FullPivHouseholderQR
  -- | Standard Cholesky decomposition (LL^T) of a matrix.
  | LLT
  -- | Robust Cholesky decomposition of a matrix with pivoting.
  | LDLT
  -- | Two-sided Jacobi SVD decomposition of a rectangular matrix.
  | JacobiSVD
  deriving (Enum, Eq, Show, Read)

con2CTag :: Decomposition -> CInt
{-# INLINE con2CTag #-}
con2CTag = \case
  PartialPivLU         -> 0
  FullPivLU            -> 1
  HouseholderQR        -> 2
  ColPivHouseholderQR  -> 3
  FullPivHouseholderQR -> 4
  LLT                  -> 5
  LDLT                 -> 6
  JacobiSVD            -> 7

-- | [x = solve d a b] finds a solution @x@ of @ax = b@ equation using decomposition @d@
solve :: (KnownNat n, KnownNat m, KnownNat n1, KnownNat m1, Elem a)
  => Decomposition
  -> Matrix n m a
  -> Matrix n1 m1 a
  -> Matrix m 1 a
solve d a b = Internal.performIO $ do
  x :: MM.IOMatrix m 1 a <- MM.new
  MM.unsafeWith x $ \x_vals x_rows x_cols ->
      unsafeWith a $ \a_vals a_rows a_cols ->
          unsafeWith b $ \b_vals b_rows b_cols ->
              Internal.call $ Internal.solve (con2CTag d)
                x_vals x_rows x_cols
                a_vals a_rows a_cols
                b_vals b_rows b_cols
  unsafeFreeze x

relativeError :: (KnownNat n, KnownNat m, KnownNat n1, KnownNat m1, KnownNat n2, KnownNat m2, Elem a)
  => Matrix n m a
  -> Matrix n1 m1 a
  -> Matrix n2 m2 a
  -> a
relativeError x a b = Internal.performIO $
  unsafeWith x $ \x_vals x_rows x_cols ->
    unsafeWith a $ \a_vals a_rows a_cols ->
      unsafeWith b $ \b_vals b_rows b_cols ->
        alloca $ \pe -> do
          Internal.call $ Internal.relativeError pe
            x_vals x_rows x_cols
            a_vals a_rows a_cols
            b_vals b_rows b_cols
          fromC <$> peek pe

rank :: (KnownNat n, KnownNat m, Elem a)
  => Decomposition
  -> Matrix n m a
  -> Int
rank d m = Internal.performIO $ alloca $ \pr -> do
  Internal.call $ unsafeWith m $ Internal.rank (con2CTag d) pr
  fromC <$> peek pr

kernel :: forall a n m. (Elem a, KnownNat n, KnownNat m)
  => Decomposition
  -> Matrix n m a
  -> Matrix n m a
kernel d m = Internal.performIO $
  alloca $ \pvals ->
  alloca $ \prows ->
  alloca $ \pcols ->
    unsafeWith m $ \vals1 rows1 cols1 -> do
      Internal.call $ Internal.kernel (con2CTag d)
        pvals prows pcols
        vals1 rows1 cols1
      vals <- peek pvals
      rs <- fromC <$> peek prows
      cs <- fromC <$> peek pcols
      fp <- FC.newForeignPtr vals $ Internal.free vals
      pure $ Matrix . Vec $ VS.unsafeFromForeignPtr0 fp (rs * cs)

image :: forall a n m. (Elem a, KnownNat n, KnownNat m)
  => Decomposition
  -> Matrix n m a
  -> Matrix n m a
image d m = Internal.performIO $
  alloca $ \pvals ->
  alloca $ \prows ->
  alloca $ \pcols ->
    unsafeWith m $ \vals1 rows1 cols1 -> do
      Internal.call $ Internal.image (con2CTag d)
        pvals prows pcols
        vals1 rows1 cols1
      vals <- peek pvals
      rs <- fromC <$> peek prows
      cs <- fromC <$> peek pcols
      fp <- FC.newForeignPtr vals $ Internal.free vals
      pure $ Matrix . Vec $ VS.unsafeFromForeignPtr0 fp (rs * cs)

{- |
[(coeffs, error) = linearRegression points] computes multiple linear regression @y = a1 x1 + a2 x2 + ... + an xn + b@ using 'ColPivHouseholderQR' decomposition

* point format is @[y, x1..xn]@

* coeffs format is @[b, a1..an]@

* error is calculated using 'relativeError'

@
import Data.Eigen.LA
main = print $ linearRegression (Row @5)
  [
    [-4.32, 3.02, 6.89],
    [-3.79, 2.01, 5.39],
    [-4.01, 2.41, 6.01],
    [-3.86, 2.09, 5.55],
    [-4.10, 2.58, 6.32]
  ]
@

 produces the following output

 @
 ([-2.3466569233817127,-0.2534897541434826,-0.1749653335680988],1.8905965120153139e-3)
 @

-}
linearRegression :: forall r. (KnownNat r)
  => Internal.Row r
  -- -> Internal.Col c
  -> [[Double]]
  -> Maybe ([Double], Double)
linearRegression _ points = do
  _a :: MatrixXd r 2 <- M.fromList $ List.map ((1:)  . tail) points
  _b :: MatrixXd r 1 <- M.fromList $ List.map ((:[]) . head) points
  let _x = solve ColPivHouseholderQR _a _b
  let e  = relativeError _x _a _b
  let coeffs = List.map head $ M.toList _x
  return (coeffs, e)
