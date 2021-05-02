module Main where

import Math

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import System.Random

render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)

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
sigmoid x = [[(1 / (1 + exp (-i))) | i <- j] | j <- x]

-- |Softmax function for a matrix
softmax :: [[Double]] -> [[Double]]
softmax x = [[(exp z) / sum (map (exp) y) | z <- y] | y <- x]


-- |TODO
sub :: Num a => [[a]] -> a -> [[a]]
sub x y = [[i - y | i <- j] | j <- x]

mul :: Num a => [[a]] -> a -> [[a]]
mul x y = [[i * y | i <- j] | j <- x]

main = do
  s <- decompress <$> BS.readFile "data/train-images-idx3-ubyte.gz"
  l <- decompress <$> BS.readFile "data/train-labels-idx1-ubyte.gz"
  let n = (random newStdGen (0, 60000))
  --let n = n - (n `mod` 28)
  --putStr . unlines $
    --[(render . BS.index s . (n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  --print $ BS.index l (n + 8)

  let x2 = softmax (sigmoid [col (layer w1 x1) 0]) -- Col layer transposes matrix upright
  let out = softmax (sigmoid [col (layer w2 x2) 0])

  print([[BS.index s (n + (x*28^2 + y*28)) | y <- [0..27]] | x <- [0..27]])
  --print(BS.index s 4)

    --print(x2)
    --print(out)

