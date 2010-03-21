module Problem2 where

import Prompt
import System.Environment

multiplesSum :: Integral a => [a] -> [a] -> a
multiplesSum ds ns = sum $ multiples ds ns

multiples :: Integral a => [a] -> [a] -> [a]
multiples ds ns = [n | n <- ns, any (multiple n) ds]

multiple :: Integral a => a -> a -> Bool
multiple n m = n `mod` m == 0

fib :: Integral a => Int -> a
fib = (map fib' [0..] !!)
  where fib' 0 = 1
        fib' 1 = 2
        fib' n = fib (n - 1) + fib (n - 2)

fibList :: Integral a => [a]
fibList = map fib [0..]

main = do
  putStrLn "Find the sum of all the multiple <ns> terms in the sequence which do not exceed <max>."
  max <- prompt "max"
  ds <- promptList "ns"
  putStrLn . show $ multiplesSum ds $ takeWhile (< max) fibList

