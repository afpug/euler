module Problem9Fast where

import Prompt
import Data.List (find)
import System.Environment (getArgs)

integralPair :: Integral a => a -> (a, a)
integralPair n = ((aFromB n b), b) where b = findB n

findB :: Integral a => a -> a
findB n = case find (isAIntegral n) [2..(n-1)] of
             Just x -> x
             Nothing -> error "No pair found"

isAIntegral :: Integral a => a -> a -> Bool
isAIntegral n b = (((n^2) `div` 2) - b*n) `mod` (n - b) == 0

aFromB :: Integral a => a -> a -> a
aFromB n b = (((n^2) `div` 2) - b*n) `div` (n - b)

tripletFromPair :: Integral a => a -> (a, a) -> (a, a ,a)
tripletFromPair n (a, b) = (a, b, n - a - b)

main = do
  putStrLn "There exists exactly one Pythagorean triplet for which a + b + c = 1000.  Find the product abc."
  sum <- prompt "sum (1000)"
  let t@(a, b, c) = tripletFromPair sum (integralPair sum)
  putStrLn (show t) 
  putStrLn ("a^2 + b^2 = c^2: " ++ show (a*a) ++ " + " ++ show (b*b) ++ " = " ++ show (c*c))
  putStrLn ("a + b + c: " ++ (show (a+b+c)))
  putStrLn ("a*b*c: " ++ (show (a*b*c)))
