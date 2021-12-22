import Data.Text (splitOn, pack, unpack)
import qualified Data.Map as M

type Cuboid = (String, [[Int]])
type Reactor = M.Map (Int, Int, Int) ()

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

cuboid :: String -> Cuboid
cuboid s = (instruction, xyz ranges)
    where [instruction, ranges] = split " " s
          xyz = map (range . drop 2) . split ","
          range :: String -> [Int]
          range = map read . split ".."

restrict :: Int -> Int -> [Int]
restrict nMin nMax = [(max (-50) nMin)..(min nMax 50)]

apply :: Reactor -> Cuboid -> Reactor
apply reactor ("on", [[xMin,xMax],[yMin,yMax],[zMin,zMax]]) = foldr ($) reactor [M.insert (x,y,z) () | x<-restrict xMin xMax, y<-restrict yMin yMax, z<-restrict zMin zMax]
apply reactor ("off", [[xMin,xMax],[yMin,yMax],[zMin,zMax]]) = foldr ($) reactor [M.delete (x,y,z) | x<-restrict xMin xMax, y<-restrict yMin yMax, z<-restrict zMin zMax]
apply reactor _ = error "Bad cuboid"

applyAll :: Reactor -> [Cuboid] -> Reactor
applyAll reactor = foldr (flip apply) reactor . reverse

main = do
    cuboids <- map cuboid . lines <$> readFile "day22.txt"
    print $ M.size $ applyAll M.empty cuboids