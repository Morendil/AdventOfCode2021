import Data.Text (splitOn, pack, unpack)
import Data.List
import Data.Maybe

type Entry = (Pattern, Output)
type Pattern = [String]
type Output = [String]
type Possibles = String
type Mapping = String

digitSegments = [6,2,5,5,4,5,6,3,7,6] -- map length segments
segments = ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","acbdefg","abcdfg"]

assignSegments :: Pattern -> [Possibles]
assignSegments patterns = map (\freq -> filter (\c -> length (filter (elem c) patterns) == freq) "abcdefg") [8,6,8,7,4,9,7]

candidates :: [Possibles] -> [Mapping]
candidates = filter (\l -> nub l == l) . sequence . assignSegments

consistent :: Pattern -> Mapping -> Bool
consistent patterns mapping = sort (map sort $ transform mapping segments) == sort (map sort patterns)

transform :: Mapping -> Pattern -> Pattern
transform mapping = map (map (\c -> mapping !! fromJust (elemIndex c "abcdefg")))

solve :: Entry -> Int
solve (patterns, output) = sum $ zipWith (\m n->m*10^n) (reverse digits) [0..]
    where mapping = head $ filter (consistent patterns) (candidates patterns)
          transformed = normalize $ transform mapping segments
          digits = map (fromJust . (`elemIndex` transformed)) (normalize output)
          normalize = map sort

part1 :: [Entry] -> Int
part1 = length . filter hasUniqueCount . concatMap snd
    where hasUniqueCount output = let l = length output in l `elem` [2,3,4,7]

parse :: String -> Entry
parse line = toPair $ map (words . unpack) $ splitOn (pack " | ") $ pack line
    where toPair (x:y:_) = (x,y)
          toPair _ = error "Not a pair"

main = do
    entries <- map parse . lines <$> readFile "day08.txt"
    print $ part1 entries
    print $ sum $ map solve entries
