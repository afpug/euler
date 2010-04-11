---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com

import Euler (primes)

euler10 = sum.takeWhile (<=2000000) $ (primes::[Integer])


main :: IO ()
main = print euler10
