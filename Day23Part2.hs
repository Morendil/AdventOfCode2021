{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Tuple
import Data.Graph.AStar
import Data.Graph
import Data.Maybe
import qualified Data.HashSet as H
import qualified Data.Map as M
import Test.Hspec
import Debug.Trace

type Slot = Int
type Energy = Int
type Pod = (Slot, Char)
type Occupancy = [Pod]
type Ways = [(Slot, Slot)]
type State = (Occupancy, Energy)
type Path = [Slot]

stops :: [Slot]
stops = reverse $ [0..26] \\ [2,4,6,8]

paths :: M.Map (Int, Int) Path
paths = M.fromList $ [((from,to), fromJust $ findPath from to) | from <- stops, to <- stops, from /= to ]
    where findPath from to = aStar around (const $ const 1) (const 1) (==to) from
            where around spot = H.fromList $ map snd $ filter ((==spot).fst) graph

graph :: Ways
graph = pairs ++ map swap pairs
    where pairs = [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10),
                   -- rooms
                   (2,11),(11,12),(12,13),(13,14),
                   (4,15),(15,16),(16,17),(17,18),
                   (6,19),(19,20),(20,21),(21,22),
                   (8,23),(23,24),(24,25),(25,26)]

display :: [(Int, Char)] -> String
display occupancy = unlines [l1,l2,l3,l4,l5]
    where l1 = unwords $ map (disp . flip lookup occupancy) [0..10]
          l2 = "    " ++ intercalate "   " (map (disp . flip lookup occupancy) [11,15..23])
          l3 = "    " ++ intercalate "   " (map (disp . flip lookup occupancy) [12,16..24])
          l4 = "    " ++ intercalate "   " (map (disp . flip lookup occupancy) [13,17..25])
          l5 = "    " ++ intercalate "   " (map (disp . flip lookup occupancy) [14,18..26])
          disp Nothing = "_"
          disp (Just c) = [c]

initial :: Occupancy
initial = [(11,'B'),(12,'D'),(13,'D'),(14,'A'),(15,'C'),(16,'C'),(17,'B'),(18,'D'),(19,'B'),(20,'B'),(21,'A'),(22,'C'),(23,'D'),(24,'A'),(25,'C'),(26,'A')]

initial2 :: Occupancy
initial2 = [(11,'B'),(12,'D'),(13,'D'),(14,'C'),(15,'C'),(16,'C'),(17,'B'),(18,'D'),(19,'A'),(20,'B'),(21,'A'),(22,'D'),(23,'B'),(24,'A'),(25,'C'),(26,'A')]

goal :: Occupancy
goal = [(11,'A'),(12,'A'),(13,'A'),(14,'A'),(15,'B'),(16,'B'),(17,'B'),(18,'B'),(19,'C'),(20,'C'),(21,'C'),(22,'C'),(23,'D'),(24,'D'),(25,'D'),(26,'D')]

properties :: Char -> (Int, Int)
properties 'A' = (1,14)
properties 'B' = (10,18)
properties 'C' = (100, 22)
properties 'D' = (1000, 26)
properties _ = error "Bad amphipod"

heuristic :: State -> Int
heuristic state = sum $ map (heuristicFor state) "ABCD"

heuristicFor :: State -> Char -> Int
heuristicFor (occupancy, _) c = cost * allDists
    where (cost, goal) = properties c
          starts = map fst $ filter ((==c).snd) occupancy
          allDists = sum (mapMaybe distanceTo starts) - 6
          distanceTo = fmap length . flip M.lookup paths . (, goal)

-- Don't move if already in room
arrived :: Occupancy -> Pod -> Bool
arrived prev (slot, c) = slot `elem` [goal-3..goal] && all (\s->(s,c) `elem` prev) [slot..goal]
    where goal = snd (properties c)

-- Don't move into a room with others not of my kind
obstructed :: Occupancy -> Pod -> Slot -> Bool
obstructed prev (_, item) slot = slot `elem` [goal-3..goal] && any (/=item) (mapMaybe (`lookup` prev) [goal-3..goal])
    where goal = snd (properties item)

move :: State -> Pod -> Slot -> Maybe State
move (prev, _) pod@(slot, c) _ | arrived prev pod = Nothing
move (prev, _) pod goal | obstructed prev pod goal = Nothing
move (prev, e) (slot, item) dest = if isJust path && all isClear thePath then result else Nothing
    where isClear pt = isNothing $ lookup pt prev
          result = Just ((dest,item):delete (slot,item) prev, e')
          path = M.lookup (slot,dest) paths
          thePath = fromJust path
          e' = e + (length thePath * fst (properties item))

moves :: State -> Pod -> [State]
-- forced moves first
moves state pod@(slot,item) | (not.null) forcedMoves = take 1 forcedMoves
    where forcedMoves = reverse $ mapMaybe (move state pod) [goal-3..goal]
          goal = snd (properties item)
-- from the rooms
moves state pod@(slot,item) | slot > 10 = mapMaybe (move state pod) [10,9,7,5,3,1,0]
-- from the hallway… nothing but forced moves
moves state@(prev,_) pod@(slot,item) = []

neighbours :: State -> [State]
neighbours state@(prev,_) = concatMap (moves state) prev

solve :: Occupancy -> Maybe [State]
solve occupancy = aStar (H.fromList . neighbours) (\(_,e1) (_,e2) -> e2-e1) heuristic ((== goal).sort.fst) (occupancy, 0)
-- solve occupancy = aStar (H.fromList . neighbours) (\(_,e1) (_,e2) -> e2-e1) heuristic ((== goal).(\x -> trace (display x) x).sort.fst) (occupancy, 0)

main = do
    print $ solve initial
    print $ solve initial2

test :: IO ()
test = hspec $ do
  describe "Heuristic" $ do
    it "moves all Ds with the right cost" $
        heuristicFor ([(11,'D'),(12,'D'),(13,'D'),(14,'D')],0) 'D' `shouldBe` 44000
  describe "Arrived" $ do
    it "won't move if in its own room and no others below" $
        arrived [(26,'D')] (26,'D') `shouldBe` True
    it "should move if one other below" $
        arrived [(25,'D'),(26,'C')] (25,'D') `shouldBe` False
    it "should move if two others below" $
        arrived [(24,'D'),(25,'C'),(26,'C')] (24,'D') `shouldBe` False
    it "won't move if in its own room and no others below" $
        arrived [(24,'D'),(25,'D'),(26,'D')] (24,'D') `shouldBe` True
  describe "Sanity" $ do
    it "should solve the easiest case" $
        let easy = [(0,'A'),(12,'A'),(13,'A'),(14,'A'),(15,'B'),(16,'B'),(17,'B'),(18,'B'),(19,'C'),(20,'C'),(21,'C'),(22,'C'),(23,'D'),(24,'D'),(25,'D'),(26,'D')]
        in length <$> solve easy `shouldBe` Just 1
    it "should solve a really easy one" $
        let easy = [(11,'D'),(12,'A'),(13,'A'),(14,'A'),(15,'B'),(16,'B'),(17,'B'),(18,'B'),(19,'C'),(20,'C'),(21,'C'),(22,'C'),(23,'A'),(24,'D'),(25,'D'),(26,'D')]
        in length <$> solve easy `shouldBe` Just 3
