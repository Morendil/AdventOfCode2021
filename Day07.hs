import Data.Text (splitOn, pack, unpack)
import Data.List
import Data.Ord
import GHC.Float

import Test.Hspec
import Test.QuickCheck

fuel :: [Int] -> Int -> Int
fuel crabs pos = sum $ map (abs . (pos -)) crabs

varfuel :: [Int] -> Int -> Int
varfuel crabs pos = sum $ map (triangle . abs . (pos -)) crabs
  where triangle n = (n * (n+1)) `div` 2

part1_brute :: [Int] -> Int
part1_brute crabs = minimum $ map (fuel crabs) [minimum crabs..maximum crabs]

part2_brute :: [Int] -> Int
part2_brute crabs = minimum $ map (varfuel crabs) [minimum crabs..maximum crabs]

part2_average :: [Int] -> Int
part2_average crabs = min (varfuel crabs average) (varfuel crabs (average+1))
    where average = floor (int2Float (sum crabs) / int2Float (length crabs))

part1_median :: [Int] -> Int
part1_median crabs = fuel crabs median
    where median = head lastHalf
          half = length crabs `div` 2
          (firstHalf, lastHalf) = splitAt half $ sort crabs

solve = do
    crabs <- map (read . unpack) . splitOn (pack ",") . pack <$> readFile "day07.txt"
    print $ part1_brute crabs
    print $ part1_median crabs
    print $ part2_brute crabs
    print $ part2_average crabs

main :: IO ()
main = hspec $ do
  describe "Brute force compared to closed form" $ do
    it "returns the the same value on part 1" $
      let sameAnswer :: [Positive Int] -> Bool
          sameAnswer crabs'@(x:y:rest) = part1_brute crabs == part1_median crabs
            where crabs = map getPositive crabs'
          sameAnswer _ = True
      in property sameAnswer
    it "returns the the same value on part 2" $
      let sameAnswer :: [Positive Int] -> Bool
          sameAnswer crabs'@(x:y:rest) = part2_brute crabs == part2_average crabs
            where crabs = map getPositive crabs'
          sameAnswer _ = True
      in property sameAnswer
