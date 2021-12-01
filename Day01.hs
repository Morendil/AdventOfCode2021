import Data.List (transpose, tails)

part1 :: [Int] -> Int
part1 terms = do
    length $ filter (uncurry (>)) $ zip (tail terms) terms

part2 :: [Int] -> Int
part2 terms = part1 $ map sum $ windows 3 terms

windows :: Int -> [a] -> [[a]]
windows n = transpose . take n . tails

main = do
    terms <- map read . lines <$> readFile "day01.txt"
    print $ part1 terms
    print $ part2 terms