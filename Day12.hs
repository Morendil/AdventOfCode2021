import Data.Text (splitOn, pack, unpack)
import Data.Char

type Edge = (String, String)
type Node = String
type Path = [Node]

pair :: String -> [String]
pair = map unpack . splitOn (pack "-") . pack

toEdges pair@[from,to] = [(from,to),(to,from)]
toEdges _ = error "Pairs only"

paths1 :: [Edge] -> [Path] -> [Node] ->  Node -> [Path]
paths1 _ starts _ "end" = map (++ ["end"]) starts
paths1 edges starts visited current = concatMap (paths1 edges starts' visited') next
    where next = map snd $ filter (not.(`elem` visited).snd) $ filter ((current ==).fst) edges
          visited' = if all isLower current then current:visited else visited
          starts' = map (++[current]) starts

paths2 :: [Edge] -> [Path] -> [Node] ->  [Node] -> Node -> [Path]
paths2 _ starts _ _ "end" = map (++ ["end"]) starts
paths2 edges starts visited twice current = concatMap (paths2 edges starts' visited' twice') next
    where next = map snd $ filter (canReach.snd) $ filter ((current ==).fst) edges
          starts' = map (++[current]) starts
          visited' = if all isLower current then current:visited else visited
          canReach cave = null twice' || cave `notElem` visited
          twice' = if null twice && current `elem` visited && all isLower current then [current] else twice

main = do
    pairs <- map pair . lines <$> readFile "day12.txt"
    let edges = filter ((/="end").fst) $ filter ((/="start").snd) $ concatMap toEdges pairs
    -- part1
    print $ length $ paths1 edges [[]] [] "start"
    -- part2
    print $ length $ paths2 edges [[]] [] [] "start"
