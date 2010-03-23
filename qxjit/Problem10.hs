module Problem10 where

import Prompt
import Data.Int

-- Can stop eliminating divisors at sqrt n.  http://mathworld.wolfram.com/SieveofEratosthenes.html
primesUpTo max = 2 : primes [3, 5..max]
  where primes (next:rest) | next <= (floor $ sqrt (fromIntegral max)) = next : primes [n | n <- rest, n `mod` next /= 0]
        primes all = all

main = do
  putStrLn "Find the sum of all the primes below <max>."
  max <- prompt "max" :: IO Int64
  putStrLn $ show (sum (primesUpTo max))
