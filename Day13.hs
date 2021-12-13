import Data.Text (splitOn, pack, unpack)
import Data.List
import qualified Data.Map as M

type Fold = (Int, Int) -> (Int, Int)

pair :: String -> (Int, Int)
pair = toPair . parts
    where parts =split ","
          toPair [x,y] = (read x, read y)
          toPair _ = error "Nope"

toFold :: String -> Fold
toFold entry = if dir == 'y' then foldy line else foldx line
    where [part1, part2] = split "=" entry
          dir = last part1
          line = read part2

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

foldy :: Int -> (Int, Int) -> (Int, Int)
foldy n (x,y) = if y > n then (x,2*n-y) else (x,y)

foldx :: Int -> (Int, Int) -> (Int, Int)
foldx n (x,y) = if x > n then (2*n-x,y) else (x,y)

display :: [(Int, Int)] -> [String]
display coords = [ [ M.findWithDefault '.' (x,y) grid | x <- [0..xMax]] | y <- [0..yMax]]
  where xMax = maximum $ map fst $ M.keys grid
        yMax = maximum $ map snd $ M.keys grid
        grid = M.fromList $ zip coords (repeat '#')


main = do
    [dots, instructions] <- split "\n\n" <$> readFile "day13.txt"
    let coords = map pair $ split "\n" dots
        folds = map toFold $ split "\n" instructions
    -- part1
    print $ length $ nub $ map (head folds) coords
    -- part2
    putStrLn $ unlines $ display $ nub $ foldl (flip map) coords folds
