{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Eigen.Matrix
import Eigen.Solver.LA
import Refined

list :: Refined (SizeEqualTo 3) [Refined (SizeEqualTo 3) [Float]]
list = $$(refineTH [$$(refineTH [1,2,5]) :: Refined (SizeEqualTo 3) [Float], $$(refineTH [2,1,4]), $$(refineTH [3,0,3])])

a :: MatrixXf 3 3
a = fromList list

main :: IO ()
main = do
    putStrLn "Here is the matrix A:"
    print a

    putStrLn "The rank of A is:"
    print $ rank FullPivLU a

    putStrLn "Here is a matrix whose columns form a basis of the null-space of A:"
    print $ kernel FullPivLU a

    putStrLn "Here is a matrix whose columns form a basis of the column-space of A:"
    print $ image FullPivLU a