{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Eigen.Matrix
import Eigen.Solver.LA
import Refined

list1 :: Refined (SizeEqualTo 3) [Refined (SizeEqualTo 3) [Double]]
list1 = $$(refineTH [$$(refineTH [1,2,3]) :: Refined (SizeEqualTo 3) [Double], $$(refineTH [4,5,6]), $$(refineTH [7,8,9])])

list2 :: Refined (SizeEqualTo 3) [Refined (SizeEqualTo 1) [Double]]
list2 = $$(refineTH [$$(refineTH [3]) :: Refined (SizeEqualTo 1) [Double], $$(refineTH [3]), $$(refineTH [4])])

solution :: MatrixXd 3 1
solution =
  let
    (_a :: MatrixXd 3 3) = fromList list1
    (_b :: MatrixXd 3 1) = fromList list2
  in solve ColPivHouseholderQR _a _b

main :: IO ()
main = print solution