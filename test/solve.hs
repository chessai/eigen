{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Eigen.Matrix
import Eigen.Solver.LA
import Data.Maybe (fromMaybe)

solution :: Maybe (MatrixXd 3 1)
solution = do
  _a :: MatrixXd 3 3 <- fromList [[1,2,3], [4,5,6], [7,8,9]]
  _b :: MatrixXd 3 1 <- fromList [[3],[3],[4]]
  pure $ solve ColPivHouseholderQR _a _b

main :: IO ()
main = print solution