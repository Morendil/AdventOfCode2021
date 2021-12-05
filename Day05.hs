{-# Language ParallelListComp #-}

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Data.List

type Coord = (Int, Int)
type Line = (Coord, Coord)

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)
coord :: ReadP Coord
coord = (,) <$> number <*> (string "," *> number)
ventLine :: ReadP Line
ventLine = (,) <$> coord <*> (string " -> " *> coord)
ventLines :: ReadP [Line]
ventLines = sepBy1 ventLine (string "\n")

parse :: ReadP a -> String ->  a
parse parser input =
    case reverse $ readP_to_S parser input of
        ((result, _):_) -> result
        _ -> error "No parse"

range :: (Enum a, Ord a) => (a,a) -> [a]
range (a,b) | a > b = reverse [b..a]
range (a,b) | a == b = repeat a
range (a,b) = [a..b]

expand :: Line -> [Coord]
expand ((x1,y1),(x2,y2)) = [(x,y)| x <- range (x1,x2) | y <- range (y1,y2)]

horv :: Line -> Bool
horv ((x1,y1),(x2,y2)) = x1==x2 || y1 == y2

main = do
    vents <- parse ventLines <$> readFile "day05.txt"
    let danger = length . filter (\g -> length g > 1) . group . sort . concat
    let for_part1 = map expand $ filter horv vents
    print $ danger for_part1
    let for_part2 = map expand vents
    print $ danger for_part2
