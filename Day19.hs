import Data.Text (splitOn, pack, unpack)
import Data.List
import Data.List.HT (takeUntil)
import Data.Maybe
import Data.Ord
import Control.Monad (replicateM)
import qualified Data.Map as M

type Permutation = [Int]
type Point = [Int]
type Scanner = [[Int]]
type Fingerprint = [[Int]]
type Position = (Int, Int, Int)
type Distances = [(Int,([Int],[Int]))]
type Enriched = (Scanner, Distances)
type Orientation = [(Int, Int)]
type Solution = M.Map Int (Scanner, Point)

add :: Point -> Point -> Point
add [x1,y1,z1] [x2,y2,z2] = [x1+x2,y1+y2,z1+z2]
add _ _ = error "Bad point"

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

triplet :: String -> [Int]
triplet = map read . split ","

orientations :: [Orientation]
orientations = nub [zip d p | p <- axesPermutations, d <- directions]
    where axesPermutations = permutations [0,1,2]
          directions = replicateM 3 [1,-1]

transform :: Scanner -> Orientation -> Scanner
transform scanner = transpose . map transformAxis
    where transformAxis (dir,n) = map (*dir) $ transpose scanner !! n

allPairs :: [a] -> [(a, a)]
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

distances :: Scanner -> Distances
distances scanner = [((x1-x2)^2+(y1-y2)^2+(z1-z2)^2,(p1,p2)) | (p1@[x1,y1,z1], p2@[x2,y2,z2]) <- allPairs scanner]

enrich :: Scanner -> Enriched
enrich scanner = (scanner, distances scanner)

match :: Distances -> Scanner -> Maybe (Orientation, Point)
match reference scanner = if length matches /= 66 then Nothing else Just (adjust $ head candidates)
    where sameDist a b = fst a == fst b
          dists = distances scanner
          matches = intersectBy sameDist dists reference
          fromRef = points $ intersectBy sameDist reference matches
          fromNew = points matches
          points = nub . concatMap ((\(a,b)->[a,b]).snd)
          candidates = filter good $ map (\o -> (o, try $ transform fromNew o)) orientations
          good (o, trial) = all ((==1) . length) trial
          adjust (o, trial) = (o, map head trial)
          try points = zipWith delta oldAxes (axes points)
          axes = map sort . transpose
          oldAxes = axes fromRef
          delta old new = nub $ zipWith (-) old new

match2 reference scanner = if length matches /= 66 then Nothing else Just candidates
    where sameDist a b = fst a == fst b
          dists = distances scanner
          matches = intersectBy sameDist dists reference
          fromRef = points $ intersectBy sameDist reference matches
          fromNew = points matches
          points = nub . concatMap ((\(a,b)->[a,b]).snd)
          candidates = filter good $ map (\o -> (o, try $ transform fromNew o)) orientations
          good (o, trial) = all ((==1) . length) trial
          adjust (o, trial) = (o, map head trial)
          try points = zipWith delta oldAxes (axes points)
          axes = map sort . transpose
          oldAxes = axes fromRef
          delta old new = nub $ zipWith (-) old new
initial :: [Scanner] -> Solution
initial scanners = M.insert 0 (head scanners, [0,0,0]) M.empty

solveStep :: [Scanner] -> (Solution, [(Int, Int)]) -> (Solution, [(Int, Int)])
solveStep scanners (solution, pairs) = (newSolution, remainingPairs)
    where candidatePairs = filter (\(a,b) -> isJust $ M.lookup a solution) pairs
          remainingPairs = filter (\(a,b) -> isNothing $ M.lookup b newSolution) (pairs \\ candidatePairs)
          distMap = M.fromList $ map (\n -> (n, distances $ fst $ fromJust $ M.lookup n solution)) (nub $ map fst candidatePairs)
          newSolution = foldl solvePair solution candidatePairs
          solvePair solution (from, to) = if isNothing found then solution else solution'
              where found = match (fromJust $ M.lookup from distMap) (scanners !! to)
                    (o, p) = fromJust found
                    (_, origin) = fromJust $ M.lookup from solution
                    solution' = M.insert to solved solution
                    solved = (map (add p) $ transform (scanners !! to) o, p)

solved :: (Solution, [(Int, Int)]) -> Bool
solved (_, []) = True
solved _ = False

main = do
    scanners <- map (map triplet . tail . lines) . split "\n\n" <$> readFile "day19_test.txt"
    let (solution,_) = last $ takeUntil solved $ iterate (solveStep scanners) (initial scanners, [(x,y) | x <- [0..length scanners-1], y <- [0..length scanners-1], x/=y])
        beacons = nub $ concatMap fst $ M.elems solution
    print $ M.keys solution
    print $ map snd $ M.elems solution
    print $ length beacons
    -- putStrLn $ unlines $ map show $ sort beacons
