module Math where


import System.Random
newRand = randomIO :: IO Int

m1 = Matrix [[1, 2, 3, 4],
            [1, 2, 3, 4],
            [1, 2, 3, 4],
            [1, 2, 3, 4]]

m2 = Matrix [[1, 2, 3, 4],
            [1, 2, 3, 4],
            [1, 2, 3, 4],
            [1, 2, 3, 4]]


data Matrix a = Vec a | Matrix a deriving (Eq, Show)


-- VECTOR OPERATIONS --
-- |Change fixity
--infixr 5 .+
infixr 5 .-
infixr 6 ./
infixr 6 `hadamard`

--instance Show Tensor where
    --show Vec = "Vec"
    --show Matrix = "Matrix"

stringify :: Matrix a -> [a] -> [a]
stringify _ [] = []
stringify t (x:xs) = [x] ++ stringify t xs


-- VECTOR OPERATIONS --
-- |Add two vectors
(.+) :: (Num a) => [a] -> [a] -> [a] 
[] .+ [] = []
(x:xs) .+ (y:ys) = [x + y] ++ xs .+ ys

-- |Subtract two vectors
(.-) :: (Num a) => [a] -> [a] -> [a] 
[] .- [] = []
(x:xs) .- (y:ys) = [x - y] ++ xs .- ys

-- |Divide two vectors
(./) :: [Int] -> [Int] -> [Int] 
[] ./ [] = []
(x:xs) ./ (y:ys) = [x `div` y] ++ xs ./ ys

-- |Take the Haddamard product of two vectors 
hadamard :: (Num a) => [a] -> [a] -> [a] 
[] `hadamard` [] = []
(x:xs) `hadamard` (y:ys) = [x * y] ++ xs `hadamard` ys

-- |Take the dot product of two vectors 
dot :: (Num a) => [a] -> [a] -> a 
[] `dot` [] = 0
_ `dot` [] = 0
[] `dot` _ = 0
(x:xs) `dot` (y:ys) = (x * y) + xs `dot` ys

-- |Create a matrix of size (m, n) with random doubles from 0 to 1
init_vec :: Int -> [Double]
init_vec seed = randoms (mkStdGen seed) :: [Double]


-- MATRIX OPERATIONS --
-- |Get the array of a matrix(m) at row(r)
col :: (Num a) => [[a]] -> Int -> [a]
col m r = [l !! r | l <- m]

-- |Get the array of a matrix(m) at col(r)
row :: (Num a) => [[a]] -> Int -> [a]
row m c = m !! c

-- |Take the transpose of a matrix
transpose :: (Num a) => [[a]] -> [[a]]
transpose m = [col m x | x <- [0..(length m) - 1]]

-- |Multiply two matrices(x, y) of type Int
matmul :: (Num a) => [[a]] -> [[a]] -> [[a]]
matmul _ [] = []
matmul m1 (v2:m2) = [[v1 `dot` v2 | v1 <- m1]] ++ matmul m1 m2

init_matrix :: Int -> Int -> [[Double]]
init_matrix m n = [take n (init_vec s) | s <- [1..m]]


