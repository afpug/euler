module Problem6 where

import Prompt
import Data.List (sum)
import System.Environment (getArgs)

sumSquares :: (Integral a) => [a] -> a
sumSquares xs = sum $ map square xs
  where square x = x*x

squareSum :: (Integral a) => [a] -> a
squareSum xs = s * s where s = sum xs

squareSumDifference :: (Integral a) => [a] -> a
squareSumDifference xs = (squareSum xs) - (sumSquares xs)

main = do
  putStrLn "Find the difference between the sum of the squares of the first one <count> natural numbers and the square of the sum."
  max <- prompt "count"
  putStrLn . show $ squareSumDifference [1..max]
