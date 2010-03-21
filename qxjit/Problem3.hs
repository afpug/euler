module Problem3 where

import Prompt
import Data.List (find)
import Data.Foldable (foldr)
import qualified Data.Foldable as F
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

instance F.Foldable Factorization where
  foldr step v (Prime a) = step a v
  foldr step v (Composite a b) = F.foldr step (F.foldr step v b) a

maxFactor :: Integral a => a -> a
maxFactor n = F.foldr max 0 (factor n)

main = do
  putStrLn "What is the largest prime factor of the number <n>" -- 600851475143
  n <- prompt "n"
  putStrLn . show $ maxFactor n
