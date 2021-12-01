part1 terms = do
    print $ length $ filter (uncurry (>)) $ zip (tail terms) terms

part2 terms = part1 $ zipWith (+) (tail $ tail terms) (zipWith (+) (tail terms) terms)

main = do
    contents <- readFile "day01.txt"
    let terms :: [Int]
        terms = map read $ lines contents
    part1 terms
    part2 terms