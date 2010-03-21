module Prompt where

import System.IO

prompt :: Read a => String -> IO a
prompt message = do
  putStr (message ++ ": ")
  hFlush stdout
  line <- getLine
  return $ read line

promptList :: Read a => String -> IO [a]
promptList message = do
  putStr (message ++ ": ")
  hFlush stdout
  line <- getLine
  return $ map read (words line)
