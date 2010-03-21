import qualified Problem1 as Problem1
import qualified Problem2 as Problem2
import qualified Problem3 as Problem3
import qualified Problem4 as Problem4
import qualified Problem5 as Problem5
import qualified Problem6 as Problem6
import qualified Problem7 as Problem7
import qualified Problem8 as Problem8
import qualified Problem9Fast as Problem9Fast
import qualified Problem10 as Problem10

import Prompt
import Control.Monad (forever)

main = do
  n <- prompt "Problem Number"
  forever $ do
    case n of
      1 -> Problem1.main
      2 -> Problem2.main
      3 -> Problem3.main
      4 -> Problem4.main
      5 -> Problem5.main
      6 -> Problem6.main
      7 -> Problem7.main
      8 -> Problem8.main
      9 -> Problem9Fast.main
      10 -> Problem10.main
      otherwise -> putStrLn "Unknown Problem" >> main
