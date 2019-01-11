{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative (liftA2)
import Eigen.Matrix as M
import Eigen.Solver.LA
import Data.List as List
import Control.Monad
import Data.Maybe (fromMaybe)
import Refined

list1 :: Refined (SizeEqualTo 5) [Refined (SizeEqualTo 3) [Double]]
list1 = $$(refineTH
  [ $$(refineTH [1,3.02,6.89]) :: Refined (SizeEqualTo 3) [Double]
  , $$(refineTH [1,2.01,5.39])
  , $$(refineTH [1,2.41,6.01])
  , $$(refineTH [1,2.09,5.55])
  , $$(refineTH [1,2.58,6.32])
  ])

list2 :: Refined (SizeEqualTo 5) [Refined (SizeEqualTo 1) [Double]]
list2 = $$(refineTH
  [ $$(refineTH [-4.32]) :: Refined (SizeEqualTo 1) [Double]
  , $$(refineTH [-3.79])
  , $$(refineTH [-4.01])
  , $$(refineTH [-3.86])
  , $$(refineTH [-4.10])
  ])

list3 :: Refined (SizeEqualTo 3) [Refined (SizeEqualTo 3) [Double]]
list3 = $$(refineTH
  [ $$(refineTH [0.680,0.597,-0.330]) :: Refined (SizeEqualTo 3) [Double]
  , $$(refineTH [-0.211,0.823,0.536])
  , $$(refineTH [0.566,-0.605,-0.444])
  ])

list4 :: Refined (SizeEqualTo 5) [Refined (SizeEqualTo 3) [Double]]
list4 = $$(refineTH
  [ $$(refineTH [-4.32, 3.02, 6.89]) :: Refined (SizeEqualTo 3) [Double]
  , $$(refineTH [-3.79, 2.01, 5.39]) 
  , $$(refineTH [-4.01, 2.41, 6.01]) 
  , $$(refineTH [-3.86, 2.09, 5.55]) 
  , $$(refineTH [-4.10, 2.58, 6.32]) 
  ])

a = fromList list1
b = fromList list2

_a = fromList list3
_b = M.inverse _a

main :: IO ()
main = do
  print a
  print b
  
  forM_ [FullPivLU, HouseholderQR, ColPivHouseholderQR, FullPivHouseholderQR, JacobiSVD] $ \d -> do
    let x :: MatrixXd 3 1
        x = solve d a b
        e :: Double
        e = relativeError x a b
        e' :: Double
        e' =  (norm $ sub (mul a x) b) / (norm b)
    putStrLn $ replicate 20 '*'
    print x
    print e
    print e'
    putStrLn "\n-2.34666 - 0.25349 x1 - 0.174965 x2"
    putStrLn "done\n"

  print $ (identity :: MatrixXd 4 4)
  print $ M.normalize a
  print $ M.transpose a

  print _a
  print _b
  print $ M.mul _a _b

  print $ linearRegression list4