{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Eigen.Matrix
import Eigen.Solver.LA

a :: Maybe (MatrixXf 3 3)
a = fromList [[1,2,5],[2,1,4],[3,0,3]]

main :: IO ()
main = do
    putStrLn "Here is the matrix A:"
    print a

    putStrLn "The rank of A is:"
    print $ rank FullPivLU <$> a

    putStrLn "Here is a matrix whose columns form a basis of the null-space of A:"
    print $ kernel FullPivLU <$> a

    putStrLn "Here is a matrix whose columns form a basis of the column-space of A:"
    print $ image FullPivLU <$> a