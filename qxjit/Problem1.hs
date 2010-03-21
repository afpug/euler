module Problem1 where

import Prompt
import System.Environment

multiplesSum :: Integral a => [a] -> [a] -> a
multiplesSum ds ns = sum $ multiples ds ns

multiples :: Integral a => [a] -> [a] -> [a]
multiples ds ns = [n | n <- ns, any (multiple n) ds]

multiple :: Integral a => a -> a -> Bool
multiple n m = n `mod` m == 0

main = do
  putStrLn "Add all the natural numbers below one <max> that are multiples of <ints>"
  max <- prompt "max"
  ds <- promptList "ints"
  putStrLn . show $ (multiplesSum ds [1..(max - 1)])
