import Data.Text (splitOn, pack, unpack)
import Data.List
import Control.Monad (replicateM)

type Orientation = [(Int->Int, Int)]
type Scanner = [[Int]]
type Fingerprint = [[Int]]

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

triplet :: String -> [Int]
triplet = map read . split ","

separations :: [Int] -> [Int]
separations axis = zipWith (-) sorted (tail sorted)
    where sorted = sort axis

orientations :: [Orientation]
orientations = [zip d p | p <- axesPermutations, d <- directions]
    where axesPermutations = permutations [0,1,2]
          directions = replicateM 3 [id, negate]

transform :: Scanner -> Orientation -> Scanner
transform scanner = transpose . map transformAxis
    where transformAxis (fn,n) = map fn $ transpose scanner !! n

fingerprint :: Scanner -> Fingerprint
fingerprint s = map separations $ transpose s

score :: Fingerprint -> Scanner -> Int
score f1 s = sum $ map length $ zipWith intersect f1 (fingerprint s)

allPairs :: [a] -> [(a, a)]
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

distances :: Scanner -> [(Int,([Int],[Int]))]
distances scanner = [((x1-x2)^2+(y1-y2)^2+(z1-z2)^2,(p1,p2)) | (p1@[x1,y1,z1], p2@[x2,y2,z2]) <- allPairs scanner]

main = do
    scanners <- map (map triplet . tail . lines) . split "\n\n" <$> readFile "day19_test.txt"
    print scanners
