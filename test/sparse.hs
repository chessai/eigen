{-# LANGUAGE DataKinds, TemplateHaskell, ScopedTypeVariables #-}

import Eigen.Internal
import Eigen.SparseMatrix
import Data.List (zip3, concat)
import Refined
import Foreign.C.Types

import Instances.TH.Lift ()

import qualified Data.Vector.Storable as VS

instance Lift CTriplet where
  lift c = [| |]

list1 :: Refined (SizeEqualTo 12) [(Int, Int, Double)]
list1 = $$(refineTH
  [ (0,0,0)
  , (1,1,2)
  , (2,2,1)
  , (4,4,1)
  , (5,5,0)
  , (6,6,1)
  , (10,10,1)
  , (11,11,2)
  , (12,12,2)
  , (15,15,2)
  , (16,16,1)
  , (17,17,-3)
  ])

list2 :: Refined (SizeEqualTo 1) [Refined (SizeEqualTo 3) [Double]]
list2 = $$(refineTH
  [ $$(refineTH [1,2,3]) :: Refined (SizeEqualTo 3) [Double]
  ])

a :: SparseMatrix 4 3 Double
a = fromVectorSized
  ($$(refineTH 4) :: Refined (EqualTo 4) Int)
  ($$(refineTH 3) :: Refined (EqualTo 3) Int)
  ($$(refineTH
       (VS.fromList $ fmap (\(i,j,k :: C Double) -> CTriplet i j k)
           [ (0,0,0)
           , (1,1,2)
           , (2,2,1)
           , (4,4,1)
           , (5,5,0)
           , (6,6,1)
           , (10,10,1)
           , (11,11,2)
           , (12,12,2)
           , (15,15,2)
           , (16,16,1)
           , (17,17,-3)
           ]
       )
     ) :: Refined (StorableVectorSizeEqualTo 12) (VS.Vector (CTriplet Double))
   )

b :: SparseMatrix 1 3 Double
b = fromDenseList list2

main :: IO ()
main = do
  putStrLn "Here is the matrix A:" >> print a
  putStrLn "Here is the matrix B:" >> print b
--  putStrLn "Here is the transpose of matrix A:" >> print (transpose a)
--  putStrLn "Here is the norm of matrix A:" >> print (norm a)
--  putStrLn "Here is the nonZeros of matrix A:" >> print (nonZeros a)
--  putStrLn "Here is the A * transpose B:" >> print (mul a (transpose b))
--  putStrLn "Here is the pruned matrix A with a huge reference" >> print (pruned 1e20 a)

--  print $ values a'
--  print $ innerIndices a'
--  print $ outerStarts a'
--  print $ innerNNZs a'

