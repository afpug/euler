---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com


import List (find)
import Maybe (fromJust)
import Euler (pythagorean)

triple = fromJust.find (\x -> 1000 `mod` sum x == 0) $ pythagorean
scale = div 1000 (sum triple)

euler9 = product . map (*scale) $ triple


main :: IO ()
main = print euler9