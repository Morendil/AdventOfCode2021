import Data.Text (splitOn, pack, unpack)
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Data.Matrix
import qualified Data.Vector as V

type Rule = (String, String)
type Pair = (Char, Char)
type Pairs = M.Map Pair Int
type Freqs = M.Map Char Int

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

toPair [x,y] = (x,y)
toPair _ = error "Nope"

generator :: [Rule] -> (Int, Int) -> Int
generator rules (y,x)
  | y == left = 1
  | y == right = 1
  | otherwise = 0
  where
      elements = sort $ nub $ concatMap fst rules
      pairs = [[x, y] | x <- elements, y <- elements]
      before@[a, b] = pairs !! (x-1)
      insert = head $ fromJust $ lookup before rules
      left = 1 + fromJust (elemIndex [a, insert] pairs)
      right = 1 + fromJust (elemIndex [insert, b] pairs)

vector :: [Rule] -> String -> [Int]
vector rules poly = map (count poly) pairs
    where count s t = sum [ 1 | r <- tails s, t `isPrefixOf` r ]
          elements = sort $ nub $ concatMap fst rules
          pairs = [[x, y] | x <- elements, y <- elements]

solveMatrix :: [Rule] -> String -> Int -> Int
solveMatrix rules template n = maximum counts - minimum counts
    where elements = sort $ nub $ concatMap fst rules
          pairs = [[x, y] | x <- elements, y <- elements]
          size = length pairs
          m = matrix size size (generator rules)
          initial = V.fromList $ vector rules template
          repeated = foldr1 multStd (replicate n m)
          final = multStd repeated (colVector initial)
          pairCounts = zip pairs (V.toList $ getMatrixAsVector final)
          counts = map (\e -> fromEnum (e == head template) + sum (map snd $ filter (\p -> last (fst p) == e) pairCounts)) elements

main = do
    [template, rest] <- split "\n\n" <$> readFile "day14.txt"
    let rules = map (toPair . split " -> ") $ lines rest
        elements = sort $ nub $ concatMap fst rules
        pairs = [[x, y] | x <- elements, y <- elements]
    print $ solveMatrix rules template 10
    print $ solveMatrix rules template 40
