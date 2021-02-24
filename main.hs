module Main where

import Math

euler = exp 1


-- Define nueral net
x1 = [[1..4]]            --Initial input 
w1 = init_matrix 20 4   --L1 weights(10, 4)
b1 = init_matrix 20 1   --L1 biases(10, 1)

w2 = init_matrix 10 20   --L1 weights(10, 4)
b2 = init_matrix 10 1   --L1 biases(10, 1)

y = [[1..10]]

v1 = [1..4]
v2 = [1..4]

-- |Basic layer architecture for the forward pass in nueral net
layer :: Num a => [[a]] -> [[a]] -> [[a]]
layer x w = matmul w x

-- |Sigmoid function for a matrix
sigmoid :: [[Double]] -> [[Double]]
sigmoid x = [[(1 / (1 + euler ** (-i))) | i <- j] | j <- x]

main = do
    --stringify mat

    let x2 = sigmoid [col (layer w1 x1) 0] -- Col layer transposes matrix upright
    let out = sigmoid (layer w2 x2)

    print(out)
