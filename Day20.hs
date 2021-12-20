import Data.Text (splitOn, pack, unpack)
import qualified Data.Map as M
import Data.Maybe

type Point = (Int, Int)
type Image = M.Map Point Char

neighbours :: [Point]
neighbours = [(-1,-1),(0,-1),(1,-1),(-1,0),(0,0),(1,0),(-1,1),(0,1),(1,1)]

add :: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

makeImage :: [[Char]] -> Image
makeImage grid = M.fromList [((x,y), (grid !! y) !! x) | x <- [0..bound], y <- [0..bound], (grid !! y) !! x == '#']
    where bound = length grid - 1

display :: Image -> [String]
display grid = [ [ fromMaybe '.' $ M.lookup (x,y) grid | x <- [xMin-3..xMax+3]] | y <- [yMin-3..yMax+3]]
  where coords = M.keys grid
        xs = map fst coords
        ys = map snd coords
        xMin = minimum xs
        xMax = maximum xs
        yMin = minimum ys
        yMax = maximum ys


enhance :: String -> String -> Maybe Char
enhance algo window = if algo !! value == '#' then Just '#' else Nothing
    where value = toDecimal $ map translate window
          translate '#' = 1
          translate '.' = 0
          translate _ = error "Bad input"

toDecimal :: [Int] -> Int
toDecimal bits = sum $ zipWith (\a b -> a * (2 ^ b)) (reverse bits) [0..]

step :: String -> Image -> Image
step algo grid = foldr ($) M.empty alterations
  where alterations = [M.alter (const $ enhance algo (window (x,y))) (x,y) | x <- [xMin-6..xMax+6], y <- [yMin-6..yMax+6]]
        window pt = map ((\cell -> M.findWithDefault '.' cell grid) . add pt) neighbours
        ((xMin,xMax),(yMin,yMax)) = bounds grid

bounds :: Image -> ((Int,Int),(Int,Int))
bounds grid = ((xMin,xMax),(yMin,yMax))
    where coords = M.keys grid
          xs = map fst coords
          ys = map snd coords
          xMin = minimum xs
          xMax = maximum xs
          yMin = minimum ys
          yMax = maximum ys

cleanup :: ((Int,Int),(Int,Int)) -> Image -> Image
cleanup ((xMin,xMax),(yMin,yMax)) = M.filterWithKey inBounds
    where inBounds (x,y) a = x > xMin-6 && x < xMax+6 && y > yMin-6 && y < yMax+6

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
    [kernel, gridLines] <- split "\n\n" <$> readFile "day20.txt"
    let grid = makeImage $ lines gridLines
        twice grid = cleanup (bounds grid) $ step kernel $ step kernel grid
    -- part 1
    let two = twice grid
    print $ length $ M.elems two
    putStrLn $ unlines $ display two
    -- part 2
    let final = last $ take 26 $ iterate twice grid
    print $ length $ M.elems final
    putStrLn $ unlines $ display final
