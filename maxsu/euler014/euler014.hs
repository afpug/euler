---------------------------------------------

-- Max Suica, 03-17-2010. max.suica@gmail.com
{-# LANGUAGE BangPatterns #-}

collatzCount n = count n 0
 where count !a !acc
         | a == 1 = succ acc
         | even a = count (div a 2) (succ acc)
         | otherwise = count ((3*a + 1) `div` 2) (acc + 2)

testRange = [800000..999999]

euler14 = snd $ maximum [(collatzCount a, a) | a <- testRange]


main :: IO ()
main = print euler14
