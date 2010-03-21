module Problem4 where

import Prompt
import Data.List (init)
import System.Environment (getArgs)

isPalindrome :: Integral a => a -> Bool
isPalindrome = isPalindrome' . show
         where isPalindrome' [] = True
               isPalindrome' [a] = True
               isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' . init . tail $ xs)

palindromes :: Integral a => [a] -> [a]
palindromes = filter isPalindrome

products :: Integral a => [a] -> [a] -> [a]
products as bs = [a * b | a <- as, b <- bs]

largestPalindromeProduct :: Integral a => [a] -> [a] -> a
largestPalindromeProduct as bs = maximum . palindromes $ products as bs

main = do
  putStrLn "Find the largest palindrome made from the product of two 3-digit numbers."
  min <- prompt "min (100)"
  max <- prompt "max (999)"
  putStrLn . show $ largestPalindromeProduct [min..max] [min..max]
