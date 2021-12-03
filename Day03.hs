import Data.List (transpose, maximumBy)
import Data.Char (digitToInt)
import Data.Ord (comparing)

import GHC.Exts

freq :: Eq a => a -> [a] -> Int
freq item = length . filter (== item)

mostCommon :: (Num a, Eq a) => [a] -> a
mostCommon inList = maximumBy (comparing (`freq` inList)) [0,1]

toDecimal :: [Int] -> Int
toDecimal bits = sum $ zipWith (\a b -> a * (2 ^ b)) (reverse bits) [0..]

sieve :: (Int -> Int) -> (Int, [[Int]]) -> (Int, [[Int]])
sieve transform (rank, ratings) = (rank+1, filtered)
  where cmp = transform $ mostCommon (transpose ratings !! rank)
        filtered = filter (\r -> r !! rank == cmp) ratings

sieveAll :: (Int -> Int) -> (Int, [[Int]]) -> Int
sieveAll fn = toDecimal . head . snd . until (\(_,r) -> length r == 1) (sieve fn) 

main = do
  ratings <- map (map digitToInt) . lines <$> readFile "day03.txt"
  let byRank = transpose ratings
      complement = (1 -)
  -- part1
      gamma = map mostCommon byRank
      epsilon = map complement gamma
  print $ toDecimal gamma * toDecimal epsilon
  -- part2
  let oxyRating = sieveAll id (0, ratings)
      co2Rating = sieveAll complement (0, ratings)
  print $ oxyRating * co2Rating
  print [length bits | bits <- head byRank, then group by id using groupWith]
