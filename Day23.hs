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
stops = reverse $ [0..18] \\ [2,4,6,8]

paths :: M.Map (Int, Int) Path
paths = M.fromList $ [((from,to), fromJust $ findPath from to) | from <- stops, to <- stops, from /= to ]
    where findPath from to = aStar around (const $ const 1) (const 1) (==to) from
            where around spot = H.fromList $ map snd $ filter ((==spot).fst) graph

graph :: Ways
graph = pairs ++ map swap pairs
    where pairs = [(0,1),(1,2),(2,11),(11,12),(2,3),(3,4),(4,13),(13,14),(4,5),(5,6),(6,15),(15,16),(6,7),(7,8),(8,9),(8,17),(17,18),(9,10)]

display :: [(Int, Char)] -> String
display occupancy = unlines [l1,l2,l3]
    where l1 = unwords $ map (disp . flip lookup occupancy) [0..10]
          l2 = "    " ++ intercalate "   " (map (disp . flip lookup occupancy) [11,13..17])
          l3 = "    " ++ intercalate "   " (map (disp . flip lookup occupancy) [12,14..18])
          disp Nothing = "_"
          disp (Just c) = [c]

initial :: Occupancy
initial = [(11,'B'),(12,'A'),(13,'C'),(14,'D'),(15,'B'),(16,'C'),(17,'D'),(18,'A')]

initial2 :: Occupancy
initial2 = [(11,'B'),(12,'C'),(13,'C'),(14,'D'),(15,'A'),(16,'D'),(17,'B'),(18,'A')]

goal :: Occupancy
goal = [(11,'A'),(12,'A'),(13,'B'),(14,'B'),(15,'C'),(16,'C'),(17,'D'),(18,'D')]

properties :: Char -> (Int, Int)
properties 'A' = (1,12)
properties 'B' = (10,14)
properties 'C' = (100, 16)
properties 'D' = (1000, 18)
properties _ = error "Bad amphipod"

heuristic :: State -> Int
heuristic state = sum $ map (heuristicFor state) "ABCD"

heuristicFor :: State -> Char -> Int
heuristicFor (occupancy, _) c = cost * bothDists
    where (cost, goal) = properties c
          starts = map fst $ filter ((==c).snd) occupancy
          bothDists = pred $ sum $ mapMaybe distanceTo starts -- one less, one slot is closer
          distanceTo = fmap length . flip M.lookup paths . (, goal)

arrived :: Occupancy -> Pod -> Bool
arrived prev (slot, c) = slot == goal || slot == goal-1 && (goal,c) `elem` prev
    where goal = snd (properties c)

move :: State -> Pod -> Slot -> Maybe State
move (prev, _) pod@(slot, c) _ | arrived prev pod = Nothing
move (prev, e) (slot, item) dest = if isJust path && all isClear thePath then result else Nothing
    where isClear pt = isNothing $ lookup pt prev
          result = Just ((dest,item):delete (slot,item) prev, e')
          path = M.lookup (slot,dest) paths
          thePath = fromJust path
          e' = e + (length thePath * fst (properties item))

moves :: State -> Pod -> [State]
-- from the rooms
moves state pod@(slot,item) | slot > 11 = if isJust priority1 then [fromJust priority1] else if isJust priority2 then [fromJust priority2] else random
    where priority1 = move state pod goal
          priority2 = move state pod (goal-1)
          goal = snd (properties item)
          random = mapMaybe (move state pod) (delete slot stops)
-- from the hallway
moves state@(prev,_) pod@(slot,item) = mapMaybe (move state pod) candidates
    where goal = snd (properties item)
          candidates = if (goal,item) `elem` prev then [goal-1] else [goal]

neighbours :: State -> [State]
neighbours state@(prev,_) = concatMap (moves state) prev

solve :: Occupancy -> Maybe [State]
solve occupancy = aStar (H.fromList . neighbours) (\(_,e1) (_,e2) -> e2-e1) heuristic ((== goal).sort.fst) (occupancy, 0)
-- solve occupancy = aStar (H.fromList . neighbours) (\(_,e1) (_,e2) -> e2-e1) heuristic ((== goal).(\x -> trace (display x) x).sort.fst) (occupancy, 0)

main = do
    -- print $ solve initial
    print $ solve initial2

test :: IO ()
test = hspec $ do
  describe "Heuristic" $ do
    it "moves both Ds with the right cost" $
        heuristicFor ([(11,'D'),(12,'D')],0) 'D' `shouldBe` 18000
    it "moves one D with the right cost" $
        heuristicFor ([(11,'D'),(18,'D')],0) 'D' `shouldBe` 8000
    it "leaves alone both Ds in the right place" $
        heuristicFor ([(17,'D'),(18,'D')],0) 'D' `shouldBe` 0
    it "works on a full example" $
        heuristic ([(11,'B'),(12,'C'),(13,'C'),(14,'D'),(15,'A'),(16,'D'),(17,'B'),(18,'A')],0) `shouldBe` 14326
  describe "Moving" $ do
    it "moves a D properly" $
        move ([(12,'D')],0) (12,'D') 18 `shouldBe` Just ([(18,'D')],10000)
    it "won't move if obstructed" $
        move ([(12,'D'),(11,'C')],0) (12,'D') 18 `shouldBe` Nothing
  describe "Moves for one pod" $ do
    it "sees the correct number of destinations for a lone pod" $
        length (moves ([(11,'B')],0) (11,'B')) `shouldBe` 14
    it "sees the correct number of destinations for an initial configuration" $
        length (moves ([(11,'B'),(12,'C'),(13,'C'),(14,'D'),(15,'A'),(16,'D'),(17,'B'),(18,'A')],0) (11,'B')) `shouldBe` 7
    it "sees the correct number of destinations for an obstructed configuration" $
        length (moves ([(11,'B'),(12,'C'),(3,'C'),(14,'D'),(15,'A'),(16,'D'),(17,'B'),(18,'A')],0) (11,'B')) `shouldBe` 2
