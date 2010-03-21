module Problem7 where

import Prompt
import Data.List (find)
import System.Environment (getArgs)

data Factorization a = Composite (Factorization a) (Factorization a)
                     | Prime a
     deriving (Show, Eq)

factor :: Integral a => a -> Factorization a
factor n = case (find (`factorOf` n) [2..maxPossibleFactor]) of
              Just a -> Composite (factor a) (factor (n `div` a))
              Nothing -> Prime n
        where maxPossibleFactor = floor (sqrt (fromIntegral n))

factorOf :: Integral a => a -> a -> Bool
m `factorOf` n = n `mod` m == 0

isPrime :: Integral a => a -> Bool
isPrime n = case factor n of
              Prime a -> True
              otherwise -> False

main = do
  putStrLn "What is the <nth> prime number?"
  nth <- prompt "nth"
  putStrLn . show $ filter isPrime [1..] !! nth
