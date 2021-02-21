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
col :: (Num a) => [[a]] -> Int -> [a]
col m r = [l !! r | l <- m]

-- |Get the array of a matrix(m) at col(r)
row :: (Num a) => [[a]] -> Int -> [a]
row m c = m !! c

-- |Take the dot product of two vectors 
dot_product :: (Num a) => [a] -> [a] -> a 
dot_product [] [] = 0
dot_product (x:xs) (y:ys) = (x * y) + dot_product xs ys

-- |Take the Haddamard product of two vectors 
haddamard :: (Num a) => [a] -> [a] -> [a] 
haddamard [] [] = []
haddamard (x:xs) (y:ys) = [x * y] ++ haddamard xs ys

-- |Take the transpose of a matrix
transpose :: (Num a) => [[a]] -> [[a]]
transpose m = [col m x | x <- [0..(length m) - 1]]

-- |Multiply two matrices(x, y) of type Int
matmul :: (Num a) => [[a]] -> [[a]] -> [[a]]
matmul _ [] = []
matmul m1 (v2:m2) = [[dot_product v1 v2 | v1 <- m1]] ++ matmul m1 m2


-- |Create a matrix of size (m, n) with random doubles from 0 to 1
init_vec :: Int -> [Double]
init_vec seed = randoms (mkStdGen seed) :: [Double]

init_matrix :: Int -> Int -> [[Double]]
init_matrix m n = [take n (init_vec s) | s <- [1..m]]


-- Define nueral net
x1 = [[1..4]]            --Initial input 
w1 = init_matrix 50 4   --L1 weights(10, 4)
b1 = init_matrix 50 1   --L1 biases(10, 1)

w2 = init_matrix 10 50   --L1 weights(10, 4)
b2 = init_matrix 10 1   --L1 biases(10, 1)


main = do
    let x2 = matmul w1 x1
    let out = matmul w2 x2

    print(out)
