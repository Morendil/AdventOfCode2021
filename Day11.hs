import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import GhcPlugins (takeUniqFromSupply)

type Dumbo = M.Map (Int, Int) Int

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
neighbours pt = map (add pt) [(-1,-1),(-1,0),(-1,1),(1,-1),(1,0),(1,1),(0,-1),(0,1)]

display :: Dumbo -> [String]
display grid = [ [ intToDigit $ fromJust $ M.lookup (x,y) grid | x <- [0..xMax]] | y <- [0..yMax]]
  where xMax = maximum $ map fst $ M.keys grid
        yMax = maximum $ map snd $ M.keys grid

spread :: Dumbo -> Dumbo
spread dumbo = M.mapWithKey fromNeighbours dumbo
    where neighborhood me = M.restrictKeys dumbo (S.fromList $ neighbours me)
          flashesAround me = M.size $ M.filter (>= 10) $ neighborhood me
          fromNeighbours me 0 = 0
          fromNeighbours me energy | energy >= 10 = 0
          fromNeighbours me energy = energy + flashesAround me

step :: Dumbo -> Dumbo
step dumbo = until quiet spread increase
    where increase = M.map (+1) dumbo
          quiet = null . M.filter (>=10)

stepAndCount :: (Dumbo, Int) -> (Dumbo, Int)
stepAndCount (dumbo, flashes) = (next, M.size (M.filter (==0) next))
    where next = step dumbo

showAt :: Int -> Dumbo -> IO ()
showAt n dumbo = putStrLn $ unlines $ display $ fst $ last $ take (n+1) $ iterate stepAndCount (dumbo, 0)

makeDumbo :: [[Int]] -> Dumbo
makeDumbo grid = M.fromList [((x,y), (grid !! y) !! x) | x <- [0..bound], y <- [0..bound]]
    where bound = length grid - 1

main = do
    grid <- map (map digitToInt) . lines <$> readFile "day11.txt"
    let dumbo = makeDumbo grid
        steps = iterate stepAndCount (dumbo, 0)
    -- part1
    print $ sum $ map snd $ take 101 steps
    -- part2
    print $ length $ takeWhile (\(_,flashes) -> flashes < 100) steps