import Data.Text (splitOn, pack, unpack)
import Data.Char

type Edge = (String, String)
type Node = String
type Path = [Node]

pair :: String -> [String]
pair = map unpack . splitOn (pack "-") . pack

toEdges pair@[from,to] = [(from,to),(to,from)]
toEdges _ = error "Pairs only"

count1 :: [Edge] -> Int -> [Node] ->  Node -> Int
count1 _ counted _ "end" = 1
count1 edges counted visited current = sum $ map (count1 edges counted visited') next
    where next = map snd $ filter (not.(`elem` visited).snd) $ filter ((current ==).fst) edges
          visited' = if all isLower current then current:visited else visited

count2 :: [Edge] -> Int -> [Node] ->  [Node] -> Node -> Int
count2 _ counted _ _ "end" = 1
count2 edges counted visited twice current = sum $ map (count2 edges counted visited' twice') next
    where next = map snd $ filter (canReach.snd) $ filter ((current ==).fst) edges
          visited' = if all isLower current then current:visited else visited
          canReach cave = null twice' || cave `notElem` visited
          twice' = if null twice && current `elem` visited && all isLower current then [current] else twice

main = do
    pairs <- map pair . lines <$> readFile "day12.txt"
    let edges = filter ((/="end").fst) $ filter ((/="start").snd) $ concatMap toEdges pairs
    -- part1
    print $ count1 edges 0 [] "start"
    -- part2
    print $ count2 edges 0 [] [] "start"
