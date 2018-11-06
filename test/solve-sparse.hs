{-# LANGUAGE DataKinds #-}

import qualified Eigen.Matrix as M
import Eigen.SparseMatrix
import Eigen.Solver.SparseLA as LA
import Control.Monad.Trans
import Data.Maybe

matrices :: Maybe (SparseMatrixXd 3 3, SparseMatrixXd 3 1)
matrices = do
  a <- fromDenseList [[1,2,3], [4,5,6], [7,8,10]]
  b <- fromDenseList [[3],[3],[4]]
  pure (a, b)

main :: IO ()
main = flip (maybe (print "WRONG! WRONG! WRONG!")) matrices $ \(a,b) -> do
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