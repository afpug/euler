---------------------------------------------
-- Max Suica, 03-16-2010. max.suica@gmail.com

import  Data.Maybe (fromJust)
import  Data.List (permutations, find, sort, inits)
import  Euler (intsqrt, prime)

digits = "123456789"

pandigital = map (read::String->Int) 
  $ concatMap permutations $ tail . inits $ digits

euler41 = fromJust . find prime . reverse . sort $ pandigital 

main :: IO ()
main = print euler41
