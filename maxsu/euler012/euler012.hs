---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com


import Data.List (findIndex)
import Data.Maybe (fromJust)
import Euler (factorCount)

   
divisors n = product . map ((+1).snd) $ factorCount n

triangleNums = scanl1 (+) [1..]

triFactor1 = concatMap ((replicate 2).divisors) [1..]

triFactor2 = tail $ concatMap ((replicate 2).divisors) [1,3..]

triDivisors = zipWith (*) triFactor1 triFactor2 

index = fromJust.findIndex (> 500) $ triDivisors

euler12 = triangleNums !! index


main :: IO ()
main = print euler12