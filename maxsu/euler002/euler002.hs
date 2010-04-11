---------------------------------------------
-- Max Suica, 0-10-2010. max.suica@gmail.com


evenFib = 2 : 8: zipWith (\a b -> a + 4*b) evenFib (tail evenFib)

euler2 = sum . takeWhile (< 4000000) $ evenFib


main :: IO ()
main = print euler2