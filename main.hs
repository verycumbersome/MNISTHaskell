module Main where

import Math

-- Define nueral net
x1 = [[1..4]]            --Initial input 
w1 = init_matrix 50 4   --L1 weights(10, 4)
b1 = init_matrix 50 1   --L1 biases(10, 1)

w2 = init_matrix 10 50   --L1 weights(10, 4)
b2 = init_matrix 10 1   --L1 biases(10, 1)

y = [[1..10]]

v1 = Vec [1..4]
v2 = Vec [1..4]


--layer :: (Num a) => [a] -> [[a]] -> [[a]]
--layer W x b = W  x .+ b

mat = Matrix [v1,
              v1,
              v1,
              v1]

main = do
    --stringify mat
    print("Hello")
    --print(v1 .+ v2)
    --print(v1 .- v2)
    --print(v1 ./ v2)
    --print(v1 `hadamard` v2)

    --let x2 = matmul w1 x1
    --let out = matmul w2 x2

    --print(out)
