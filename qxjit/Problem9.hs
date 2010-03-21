module Problem9 where

import Data.List (find)
import System.Environment (getArgs)

data Triplet a = Triplet a a a
  deriving (Show, Eq)

triplets :: (Num a, Ord a, Enum a) => a -> [Triplet a]
triplets n = [Triplet a b c | a <- [1..n], b <- [1..n], c <- [1..n], a < b, b < c, a + b + c == n]

isPythagorean :: Num a => Triplet a -> Bool
isPythagorean (Triplet a b c) = a*a + b*b == c*c

tripletProduct :: Num a => Triplet a -> a
tripletProduct (Triplet a b c) = a * b * c

pythagoreanTripletWithSum :: (Ord a, Num a, Enum a) => a -> Maybe (Triplet a)
pythagoreanTripletWithSum n = find isPythagorean $ triplets n

main = do
  args <- getArgs
  let sum = read . head $ args
      triplet = pythagoreanTripletWithSum sum
  case triplet of
      Just t@(Triplet _ _ _) -> putStrLn (show t) >> putStrLn (show (tripletProduct t))
      Nothing -> putStrLn "No Pythagorean Triplet Found"
