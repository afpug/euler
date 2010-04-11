---------------------------------------------
-- Max Suica, 03-25-2010. max.suica@gmail.com



fib = 1 : 1 : zipWith (+) fib (tail fib)
fibCount = map (length.show) fib

euler25 = succ.length.takeWhile (< 1000) $ fibCount

main :: IO ()
main = print euler25

