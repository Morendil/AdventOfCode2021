
import Data.Graph.AStar
import Data.Char
import Data.Maybe
import qualified Data.HashSet as H

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
neighbours pt = map (add pt) [(-1,0),(1,0),(0,-1),(0,1)]


at grid (x,y) | y < 0 = Nothing
at grid (x,y) | y >= length grid = Nothing
at grid (x,y) | x < 0 = Nothing
at grid (x,y) | x >= length (head grid) = Nothing
at grid (x,y) = Just $ (grid !! y) !! x

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
    