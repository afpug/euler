---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com


palindrome a = 
  show a == (reverse.show $ a)

palindromes min max =  
  [a*b | a <- [min..max], b <- [1..a], palindrome (a*b)]

euler4 = maximum $ palindromes 100 999


main :: IO ()
main = print euler4