import Data.Text (splitOn, pack, unpack)
import Data.Maybe
import Data.List
import qualified Data.Map as M

type Rule = (String, String)
type Pairs = String

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

toPair [x,y] = (x,y)
toPair _ = error "Nope"

expand :: [Rule] -> String -> String
expand rules poly = concat rest ++ [last poly]
    where rest = zipWith insert poly (tail poly)
          insert a b = a : fromJust (lookup [a,b] rules)

main = do
    [template, rest] <- split "\n\n" <$> readFile "day14_test.txt"
    let rules = map (toPair . split " -> ") $ lines rest
        expanded = last $ take 11 $ iterate (expand rules) template
        elements = sort $ map (\g -> (length g, head g)) $ group $ sort expanded
        least = fst $ head elements
        most = fst $ last elements
    -- part1
    print $ most-least
