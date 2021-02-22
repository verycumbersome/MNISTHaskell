module Math where

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

type Vec = [Double]
--data Matrix a = Matrix [Vec a] deriving (Eq, Show)

-- |Get the array of a matrix(m) at row(r)
col :: (Num a) => [[a]] -> Int -> [a]
col m r = [l !! r | l <- m]

-- |Get the array of a matrix(m) at col(r)
row :: (Num a) => [[a]] -> Int -> [a]
row m c = m !! c

--infixr 5 .+
--(.+) :: Num a => [a] -> [a] -> [a] 
--[] .+ [] = []
--(x:xs) (.+) (y:ys) = [x + y] ++ xs .+  ys

-- |Add two vectors
infixr 5 .+
(.+) :: (Num a) => [a] -> [a] -> [a] 
[] .+ [] = []
(x:xs) .+ (y:ys) = [x + y] ++ xs .+ ys

--instance Num Matrix where 
    --[] + [] = []
    --(x:xs) + (y:ys) = [x + y] ++ xs + ys

--v_add :: (Num a) => [a] -> [a] -> [a] 
--[] `v_add` [] = []
--(x:xs) `v_add` (y:ys) = [x + y] ++ xs `v_add` ys

---- |Add two vectors
--v_add :: (Num a) => [a] -> [a] -> [a] 
--[] `v_add` [] = []
--(x:xs) `v_add` (y:ys) = [x + y] ++ xs `v_add` ys

-- |Subtract two vectors
v_sub :: (Num a) => [a] -> [a] -> [a] 
[] `v_sub` [] = []
(x:xs) `v_sub` (y:ys) = [x - y] ++ xs `v_sub` ys

-- |Divide two vectors
v_div :: [Int] -> [Int] -> [Int] 
[] `v_div` [] = []
(x:xs) `v_div` (y:ys) = [x `div` y] ++ xs `v_div` ys

-- |Take the Haddamard product of two vectors 
hadamard :: (Num a) => [a] -> [a] -> [a] 
[] `hadamard` [] = []
(x:xs) `hadamard` (y:ys) = [x * y] ++ xs `hadamard` ys

-- |Take the dot product of two vectors 
dot :: (Num a) => [a] -> [a] -> a 
[] `dot` [] = 0
(x:xs) `dot` (y:ys) = (x * y) + xs `dot` ys


-- |Take the transpose of a matrix
transpose :: (Num a) => [[a]] -> [[a]]
transpose m = [col m x | x <- [0..(length m) - 1]]

-- |Multiply two matrices(x, y) of type Int
matmul :: (Num a) => [[a]] -> [[a]] -> [[a]]
matmul _ [] = []
matmul m1 (v2:m2) = [[v1 `dot` v2 | v1 <- m1]] ++ matmul m1 m2


-- |Create a matrix of size (m, n) with random doubles from 0 to 1
init_vec :: Int -> [Double]
init_vec seed = randoms (mkStdGen seed) :: [Double]

init_matrix :: Int -> Int -> [[Double]]
init_matrix m n = [take n (init_vec s) | s <- [1..m]]


