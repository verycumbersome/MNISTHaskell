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

row :: [[Int]] -> Int -> [Int]
row m r = [l !! r | l <- m]

col :: [[Int]] -> Int -> [Int]
col m c = m !! c

dot_product :: [Int] -> [Int] -> [Int]
dot_product [] [] = []
dot_product (x:xs) (y:ys) = [x * y] ++ dot_product xs ys

matmul x y = head x + length y
