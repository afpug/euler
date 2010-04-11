order x y = if x <= y then (x,y) else (y,x)

pythagoreans :: [[(Int,Int)]]            
pythagoreans = [[ order (m*m-n*n) (2*m*n) 
                  | let minn = 1 + mod m 2,
                    let maxn = min (m-1) (div 5000 m),
                    n <- [minn, minn+2 .. maxn], 
                    gcd m n == 1]
                  | m <- [2..10000]]

calcsum f n = sum.map f $ [1..n]

g m (a, b) = 
    calcsum (\k -> min (k*b) (k*a `div` 2))
     (min (2*m `div` a) (m `div` b)) 
  + calcsum (\k -> max 0 (min (k*a) (k*b `div` 2) - k*(b-a) + 1))
     (min (m `div` a) (2*m `div` b))

paths m = sum . map (g m) . concat $ take m pythagoreans
                     
bsearch goal start end 
    | start + 1 == end = end
    | paths mid < goal = bsearch goal mid end
    | otherwise        = bsearch goal start mid
   where
    mid = start + ((end - start) `div` 2)
                      

main = print $ bsearch 1000000 1 10000