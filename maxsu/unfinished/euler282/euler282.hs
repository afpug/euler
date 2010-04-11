---------------------------------------------
-- Max Suica, 03-16-2010. max.suica@gmail.com


-- Needs _plenty_ of work. Not done.
a 0 n = n + 1
a m 0 = a (m-1) 1
a m n = a (m-1) (a m (n-1))

a2 0 = succ
a2 (m+1) = iter(a2 m)

iter f 0 = f 1
iter f (n+1) = f $ iter f n

a3 0 = succ
a3 (m+1) = ((tail.iterate (a3 m) $ 1)!!)

aMod k 0 n = (n + 1) `mod` k
aMod k m 0 = aMod k (m-1) 1
aMod k m n = aMod k (m-1) (aMod k m (n-1))
