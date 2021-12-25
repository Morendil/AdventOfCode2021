import qualified Data.Map as M
import Data.Maybe

type Seafloor = M.Map (Int, Int) Char

makeSeafloor :: [[Char]] -> Seafloor
makeSeafloor grid = M.fromList [((x,y), (grid !! y) !! x) | x <- [0..maxX], y <- [0..maxY]]
    where maxY = length grid - 1
          maxX = length (head grid) - 1

east :: Seafloor -> Seafloor
east prev = M.mapWithKey advance prev
  where xMax = maximum $ map fst $ M.keys prev
        advance (x,y) '.' = if fromJust (M.lookup ((x+xMax) `mod` (xMax+1),y) prev) == '>' then '>' else '.'
        advance (x,y) '>' = if fromJust (M.lookup ((x+1) `mod` (xMax+1),y) prev) == '.' then '.' else '>'
        advance _ c = c

south :: Seafloor -> Seafloor
south prev = M.mapWithKey advance prev
  where yMax = maximum $ map snd $ M.keys prev
        advance (x,y) '.' = if fromJust (M.lookup (x,(y+yMax) `mod` (yMax+1)) prev) == 'v' then 'v' else '.'
        advance (x,y) 'v' = if fromJust (M.lookup (x,(y+1) `mod` (yMax+1)) prev) == '.' then '.' else 'v'
        advance _ c = c

display :: Seafloor -> [String]
display grid = [ [ fromJust $ M.lookup (x,y) grid | x <- [xMin..xMax]] | y <- [yMin..yMax]]
  where coords = M.keys grid
        xs = map fst coords
        ys = map snd coords
        xMin = minimum xs
        xMax = maximum xs
        yMin = minimum ys
        yMax = maximum ys

main = do
    grid <- lines <$> readFile "day25.txt"
    let seafloor = makeSeafloor grid
        moveHerds = south . east
    print $ succ $ length $ takeWhile (\a -> a /= moveHerds a) $ iterate moveHerds seafloor
