{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative (liftA2)
import Eigen.Matrix as M
import Eigen.Solver.LA
import Data.List as List
import Control.Monad
import Data.Maybe (fromMaybe)

a :: Maybe (MatrixXd 5 3)
a = fromList 
  [ [1,3.02, 6.89]
  , [1,2.01, 5.39]
  , [1,2.41, 6.01]
  , [1,2.09, 5.55]
  , [1,2.58, 6.32]
  ]

b :: Maybe (MatrixXd 5 1)
b = fromList $ List.map return [-4.32,-3.79,-4.01,-3.86,-4.10]

main :: IO ()
main = do
  print a
  print b
  
  forM_ [FullPivLU, HouseholderQR, ColPivHouseholderQR, FullPivHouseholderQR, JacobiSVD] $ \d -> do
    let x :: Maybe (MatrixXd 3 1)
        x = liftA2 (solve d) a b
    let e :: Maybe Double
        e = relativeError <$> x <*> a <*> b
        e' :: Maybe Double
        e' = liftA2 (/) (norm <$> (sub <$> (liftA2 mul a x) <*> b)) (norm <$> b)
    putStrLn $ replicate 20 '*'
    --print d
    print x
    print e
    print e'
    putStrLn "\n-2.34666 - 0.25349 x1 - 0.174965 x2"
    putStrLn "done"

  print $ (identity :: MatrixXd 4 4)
  print $ M.normalize <$> a
  print $ M.transpose <$> a

  let _a :: Maybe (MatrixXd 3 3)
      _a = M.fromList
        [ [ 0.680,  0.597, -0.330]
        , [-0.211,  0.823,  0.536]
        , [ 0.566, -0.605, -0.444]
        ]
      _b = M.inverse <$> _a
  
  print _a
  print _b
  print $ liftA2 M.mul _a _b

  print $ linearRegression (M.Row @5)
    [ [-4.32, 3.02, 6.89]
    , [-3.79, 2.01, 5.39]
    , [-4.01, 2.41, 6.01]
    , [-3.86, 2.09, 5.55]
    , [-4.10, 2.58, 6.32]
    ]
