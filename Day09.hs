import Data.Char
import Data.Maybe
import Data.List

type Point = (Int, Int)
type Grid = [[Int]]

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
neighbours pt = map (add pt) [(-1,0),(1,0),(0,-1),(0,1)]

at grid (x,y) | y < 0 = Nothing
at grid (x,y) | y >= length grid = Nothing
at grid (x,y) | x < 0 = Nothing
at grid (x,y) | x >= length (head grid) = Nothing
at grid (x,y) = Just $ (grid !! y) !! x

grow :: Grid -> [Point] -> [Point]
grow grid basin = basin ++ nub newPoints
    where newPoints = filter (\pt -> not (elem pt basin || isNothing (at grid pt) || at grid pt == Just 9) ) (concatMap neighbours basin)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
    grid <- map (map digitToInt) . lines <$> readFile "day09.txt"
    let height = length grid - 1
        width = length (head grid) - 1
        around pt = mapMaybe (at grid) (neighbours pt)
        isLowest pt = fromJust (at grid pt) < minimum (around pt)
        lowest = [(x,y) | x <- [0..width], y <- [0..height], isLowest (x,y)]
    -- part1
    print $ sum $ map (1+) $ mapMaybe (at grid) lowest
    -- part2
    let basins = map (length . (\pt -> converge (grow grid) [pt])) lowest
    print $ product $ take 3 $ reverse $ sort basins