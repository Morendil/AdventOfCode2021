import Data.Text (splitOn, pack, unpack)
import Data.List
import Data.Ord

main = do
    crabs <- map (read . unpack) . splitOn (pack ",") . pack <$> readFile "day07.txt"
    -- part1
    let fuel crabs pos = sum $ map (abs . (pos -)) crabs
    print $ minimum $ map (fuel crabs) [minimum crabs..maximum crabs]
    -- part2
    let triangle n = (n * (n+1)) `div` 2
        varfuel crabs pos = sum $ map (triangle . abs . (pos -)) crabs
    print $ minimum $ map (varfuel crabs) [minimum crabs..maximum crabs]
