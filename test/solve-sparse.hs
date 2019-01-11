{-# LANGUAGE DataKinds, ScopedTypeVariables, TemplateHaskell #-}

import qualified Eigen.Matrix as M
import Eigen.SparseMatrix
import Eigen.Solver.SparseLA as LA
import Control.Monad.Trans
import Data.Maybe
import Refined

list1 :: Refined (SizeEqualTo 3) [Refined (SizeEqualTo 3) [Double]]
list1 = $$(refineTH [$$(refineTH [1,2,3]) :: Refined (SizeEqualTo 3) [Double], $$(refineTH [4,5,6]), $$(refineTH [7,8,10])])

list2 :: Refined (SizeEqualTo 3) [Refined (SizeEqualTo 1) [Double]]
list2 = $$(refineTH [$$(refineTH [3]) :: Refined (SizeEqualTo 1) [Double], $$(refineTH [3]), $$(refineTH [4])])

a = fromDenseList list1
b = fromDenseList list2

main :: IO ()
main = do
  putStrLn "Here is the matrix A:"
  print a

  putStrLn "Here is the vector b:"
  print b

  runSolverT (SparseLU COLAMDOrdering) $ do
    compute a
    x <- solve b
    info >>= lift . print
    determinant >>= lift . print
    lift $ putStrLn "The solution is:"
    lift $ print x