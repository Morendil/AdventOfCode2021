
import Data.Graph.AStar
import Data.Char
import Data.Maybe
import qualified Data.HashSet as H

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
neighbours pt = map (add pt) [(-1,0),(1,0),(0,1),(0,-1)]

at grid (x,y) | y < 0 = Nothing
at grid (x,y) | y >= length grid = Nothing
at grid (x,y) | x < 0 = Nothing
at grid (x,y) | x >= length (head grid) = Nothing
at grid (x,y) = Just $ (grid !! y) !! x

at2 grid (x,y) | y < 0 = Nothing
at2 grid (x,y) | y >= 5 * length grid = Nothing
at2 grid (x,y) | x < 0 = Nothing
at2 grid (x,y) | x >= 5 * length (head grid) = Nothing
at2 grid (x,y) = Just value
    where x' = x `mod` length (head grid)
          y' = y `mod` length grid
          dx = x `div` length (head grid)
          dy = y `div` length grid
          value = (((grid !! y') !! x' - 1 + manhattan (0,0) (dx, dy)) `mod` 9) + 1

display :: Int -> Int -> [[Int]] -> [String]
display width height grid = [ [ intToDigit $ fromJust $ at2 grid (x,y) | x <- [0..width-1]] | y <- [0..height-1]]

manhattan (x1,y1) (x2,y2) = abs (y2-y1) + abs (x2-x1)

main = do
    grid <- map (map digitToInt) . lines <$> readFile "day15.txt"
    let height = length grid - 1
        width = length (head grid) - 1
        around pt = H.fromList $ filter (isJust . at grid) (neighbours pt)
        start = (0,0)
        goal = (width,height)
    let result = aStar around (\from to -> fromJust $ at grid to) (manhattan start) (== goal) start
    -- part1
    print $ sum $ mapMaybe (at grid) $ fromJust result
    let height2 = (5 * (height+1))-1
        width2 = (5 * (width+1))-1
        goal2 = (width2, height2)
        around2 pt = H.fromList $ filter (isJust . at2 grid) (neighbours pt)
        result2 = aStar around2 (\from to -> fromJust $ at2 grid to) (manhattan start) (== goal2) start
    print $ sum $ mapMaybe (at2 grid) $ fromJust result2
    