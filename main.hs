module Main where

import System.Random
newRand = randomIO :: IO Int

m1 :: [[Int]]
m1 = [[1, 2, 3, 4],
    [1, 2, 3, 4],
    [1, 2, 3, 4],
    [1, 2, 3, 4]]

m2 :: [[Int]]
m2 = [[1, 2, 3, 4],
    [1, 2, 3, 4],
    [1, 2, 3, 4],
    [1, 2, 3, 4]]


-- |Get the array of a matrix(m) at row(r)
col :: [[Int]] -> Int -> [Int]
col m r = [l !! r | l <- m]

-- |Get the array of a matrix(m) at col(r)
row :: [[Int]] -> Int -> [Int]
row m c = m !! c

-- |Take the dot product of two Int arrays
dot_product :: [Int] -> [Int] -> Int
dot_product [] [] = 0
dot_product (x:xs) (y:ys) = (x * y) + dot_product xs ys

-- |Take the Haddamard product of two Int arrays
haddamard :: [Int] -> [Int] -> [Int]
haddamard [] [] = []
haddamard (x:xs) (y:ys) = [x * y] ++ haddamard xs ys

-- |Take the transpose of a matrix
transpose :: [[Int]] -> [[Int]]
transpose m = [col m x | x <- [0..(length m) - 1]]

-- |Multiply two matrices(x, y) of type Int
matmul :: [[Int]] -> [[Int]] -> [[Int]]
matmul _ [] = []
matmul m1 (v2:m2) = [[dot_product v1 v2 | v1 <- m1]] ++ matmul m1 m2


-- |Create a matrix of size (m, n) with random doubles from 0 to 1
random_arr :: Int -> [Double]
random_arr seed = randoms (mkStdGen seed) :: [Double]

init_matrix :: Int -> Int -> [[Double]]
init_matrix m n = [take n (random_arr 10) | _ <- [0..m]]

main = do
    init_matrix 4 5

    print m1 
