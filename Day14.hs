import Data.Text (splitOn, pack, unpack)
import Data.Maybe
import Data.List
import qualified Data.Map as M

type Rule = (String, String)
type Pair = (Char, Char)
type Pairs = M.Map Pair Int
type Freqs = M.Map Char Int

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

toPair [x,y] = (x,y)
toPair _ = error "Nope"

expand :: [Rule] -> Pairs -> Pairs
expand rules poly = foldr ($) poly transforms
    where transforms = map (applyRule poly) rules

applyRule :: Pairs -> Rule -> (Pairs -> Pairs)
applyRule pairs ([a,b],[c]) = case M.lookup (a,b) pairs of
    Nothing -> id
    Just n -> M.alter (Just . maybe n (+n)) (a,c) . M.alter (Just . maybe n (+n)) (c,b) . M.adjust (\x->x-n) (a,b)
applyRule _ _ = error "foo"

insertPair :: Char -> Char -> Pairs -> Pairs
insertPair a b = M.alter (Just . maybe 1 succ) (a,b)

initial :: String -> Pairs
initial poly = foldr ($) M.empty insertions
    where insertions = zipWith insertPair poly (tail poly)

count :: Pairs -> Freqs
count = M.foldrWithKey (\(a,b) n -> M.alter (Just . maybe n (+n)) b) M.empty

solve :: [Rule] -> String -> Int -> Int
solve rules template n = most - least
    where expanded = last $ take (n+1) $ iterate (expand rules) (initial template)
          counted = M.adjust (+1) (head template) $ count expanded
          most = maximum $ M.elems counted
          least = minimum $ M.elems counted

main = do
    [template, rest] <- split "\n\n" <$> readFile "day14.txt"
    let rules = map (toPair . split " -> ") $ lines rest
    print $ solve rules template 10
    print $ solve rules template 40
