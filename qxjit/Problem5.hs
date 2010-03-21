module Problem5 where

import Prompt
import Data.List (find, foldl')
import System.Environment (getArgs)

main = do
  putStrLn "What is the smallest number that is evenly divisible by all of the numbers from <min> to <max>?"
  min <- prompt "min"
  max <- prompt "max"
  putStrLn . show $ foldl' lcm 1 [min..max]
