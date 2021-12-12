import Data.Text (splitOn, pack, unpack)
import Data.Char

type Edge = (String, String)
type Node = String
type Path = [Node]

pair :: String -> [String]
pair = map unpack . splitOn (pack "-") . pack

toEdges pair@[from,to] = [(from,to),(to,from)]
toEdges _ = error "Pairs only"

paths :: [Edge] -> [Path] -> [Node] ->  Node -> [Path]
paths _ starts _ "end" = map (++ ["end"]) starts
paths edges starts visited current = concatMap (paths edges starts' visited') next
    where next = map snd $ filter (not.(`elem` visited).snd) $ filter ((current ==).fst) edges
          visited' = if all isLower current then current:visited else visited
          starts' = map (++[current]) starts

main = do
    pairs <- map pair . lines <$> readFile "day12.txt"
    let edges = concatMap toEdges pairs
    -- part1
    print $ length $ paths edges [[]] [] "start"
