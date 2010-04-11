---------------------------------------------
-- Max Suica, 03-25-2010. max.suica@gmail.com

import Data.Char (digitToInt)

euler16 = sum . map digitToInt $ show (2^1000)

main :: IO ()
main = print euler16

