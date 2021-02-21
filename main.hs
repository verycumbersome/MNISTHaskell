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
dot_product :: [Int] -> [Int] -> [Int]
dot_product [] [] = []
dot_product (x:xs) (y:ys) = [x * y] ++ dot_product xs ys

-- |Take the transpose of a matrix
transpose :: [[Int]] -> [[Int]]
transpose m = [col m x | x <- [0..(length m) - 1]]

-- |Multiply two matrices(x, y) of type Int
--matmul :: [[Int]] -> [[Int]] -> [[Int]]
--matmul [] [] = 
--matmul (x:xs) (y:ys) = 
