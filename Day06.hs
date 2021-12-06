import Data.Text (splitOn, pack, unpack)

reproduce :: [Int] -> [Int]
reproduce fish = take 6 (tail fish) ++ [head fish+fish !! 7]++[fish !! 8] ++ [head fish]

simulate :: Int -> [Int] -> Int
simulate days demo = sum $ last $ take (days+1) $ iterate reproduce demo

ages :: [Int] -> [Int]
ages fish = map (\n -> length $ filter (==n) fish) [0..8]

main = do
    fish <- map (read . unpack) . splitOn (pack ",") . pack <$> readFile "day06.txt"
    -- part1
    print $ simulate 80 $ ages fish
    -- part2
    print $ simulate 256 $ ages fish
