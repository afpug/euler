---------------------------------------------
-- Max Suica, 03-17-2010. max.suica@gmail.com


digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens  = ["eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens   = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

number n 
    | n > 999        = "one thousand"
    | mod n 100 == 0 = digits !! div n 100 ++ " hundred"
    | n > 99         = number (div n 100 * 100) ++ " and " ++ number (mod n 100) 
    | mod n 10 == 0  = tens   !! div n 10
    | n > 20         = tens   !! div n 10 ++ " " ++ digits !! mod n 10
    | n > 10         = teens  !! (n - 11)
    | n < 10         = digits !! n


euler17 = length . concatMap (filter (/=' ') . number) $ [1..1000]


main :: IO ()
main = print euler17
 