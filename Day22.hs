{-# LANGUAGE TupleSections #-}
import Data.Text (splitOn, pack, unpack)
import qualified Data.Map as M
import Data.List
import Data.Maybe

type Cuboid = [[Int]]
type Counted = (Int, Cuboid)
type Instruction = (String, Cuboid)
type Reactor = M.Map (Int, Int, Int) ()

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

instruction :: String -> Instruction
instruction s = (instruction, xyz ranges)
    where [instruction, ranges] = split " " s
          xyz = map (range . drop 2) . split ","
          range :: String -> [Int]
          range = map read . split ".."

restrict :: Int -> Int -> [Int]
restrict nMin nMax = [(max (-50) nMin)..(min nMax 50)]

apply :: Reactor -> Instruction -> Reactor
apply reactor ("on", [[xMin,xMax],[yMin,yMax],[zMin,zMax]]) = foldr ($) reactor [M.insert (x,y,z) () | x<-restrict xMin xMax, y<-restrict yMin yMax, z<-restrict zMin zMax]
apply reactor ("off", [[xMin,xMax],[yMin,yMax],[zMin,zMax]]) = foldr ($) reactor [M.delete (x,y,z) | x<-restrict xMin xMax, y<-restrict yMin yMax, z<-restrict zMin zMax]
apply reactor _ = error "Bad cuboid"

applyAll :: Reactor -> [Instruction] -> Reactor
applyAll reactor = foldr (flip apply) reactor . reverse

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection ranges1 ranges2 = if all ((> 1).length) rgs then Just rgs else Nothing
    where rgs = zipWith inter ranges1 ranges2
          inter [min1,max1] [min2,max2] = if max min1 min2 <= min max1 max2 then [max min1 min2, min max1 max2] else []
          inter _ _ = error "Bad range"

applyOne :: [Counted] -> Instruction -> [Counted]
applyOne visited ("on", cuboid) = visited ++ (1,cuboid) : mapMaybe (\(c,o) -> (-c,) <$> intersection cuboid o) visited
applyOne visited ("off", cuboid) = visited ++ mapMaybe (\(c,o) -> (-c,) <$> intersection cuboid o) visited
applyOne _ _ = error "Bad instruction"

size :: Cuboid -> Integer
size c = product $ map (\[a,b] -> toInteger b - toInteger a + 1) c

count :: Counted -> Integer
count (n, c) = toInteger n * size c

main = do
    instructions <- map instruction . lines <$> readFile "day22.txt"
    let center = M.size $ applyAll M.empty instructions
    print center
    let final = map count $ foldl applyOne [] (drop 20 instructions)
    print $ toInteger center + sum final