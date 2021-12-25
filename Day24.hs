import Data.Text (splitOn, pack, unpack)
import qualified Data.Map as M
import Data.Maybe
import Data.List.Index
import Test.Hspec
import Test.QuickCheck

type Registers = M.Map String Int
type Instruction = [String]
type Section = [Instruction]

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

apply :: Registers -> [String] -> Registers
apply regs [opcode,arg1,arg2] = M.insert arg1 (op opcode v1 v2) regs
    where v1 = M.findWithDefault 0 arg1 regs
          v2 = if arg2 `elem` ["x","y","z","w"] then M.findWithDefault 0 arg2 regs else read arg2
          op :: String -> (Int -> Int -> Int)
          op "mul" = (*)
          op "add" = (+)
          op "mod" = mod
          op "div" = div
          op "eql" = \a b -> fromEnum $ a == b
          op _ = error "Bad op"
apply _ _ = error "Bad instruction"

applyAll :: Registers -> Section -> Registers
applyAll = foldl apply

compute :: [Int] -> [Section] -> Int
compute digits sections = fromJust $ M.lookup "z" final
    where computeSection input section = flip applyAll section . M.insert "w" input
          final = foldl (flip ($)) M.empty (zipWith computeSection digits sections)

formula :: [Int] -> Int
formula digits = sum $ zipWith (\a b -> a * (26 ^ b)) (reverse $ del1 $ del2 $ del3 real) [0..]
  where del3 l = if length l >= 9 then (if digits !! 7 - 4 == digits !! 8 then deleteAt 7 $ deleteAt 8 real else deleteAt 7 real) else real
        del2 l = if length l >= 8 then (if digits !! 6 - 4 == digits !! 7 then deleteAt 6 $ deleteAt 7 real else deleteAt 6 real) else real
        del1 l = if length l >= 6 then (if digits !! 4 == digits !! 5 then deleteAt 4 $ deleteAt 5 l else deleteAt 4 l) else l
        real = zipWith (+) digits adds
        adds = [13,16,2,8,11,6,12,2,10]

-- sameAnswer digits = compute digits sections == if (digits !! 6 - 4) /= digits !! 7 then fork1 else fork2
--   where z4 = (((head digits + 13) * 26 + digits !! 1 + 16) * 26 + digits !! 2 + 2) * 26 + digits !! 3 + 8
--         z6 = if digits !! 4 /= digits !! 5 then z4 * 26 + digits !! 5 + 6 else z4
--         fork1 = z6 * 26 + digits !! 7 + 2
--         fork2 = z6

main = do
    sections <- map (tail . map words . lines) . split "\n\n" <$> readFile "day24.txt"
    print sections

sections = [[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","1"],["add","x","15"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","13"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","1"],["add","x","10"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","16"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","1"],["add","x","12"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","2"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","1"],["add","x","10"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","8"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","1"],["add","x","14"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","11"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","26"],["add","x","-11"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","6"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","1"],["add","x","10"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","12"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","26"],["add","x","-16"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","2"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","26"],["add","x","-9"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","2"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","1"],["add","x","11"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","15"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","26"],["add","x","-8"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","1"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","26"],["add","x","-8"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","10"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","26"],["add","x","-10"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","14"],["mul","y","x"],["add","z","y"]],[["mul","x","0"],["add","x","z"],["mod","x","26"],["div","z","26"],["add","x","-9"],["eql","x","w"],["eql","x","0"],["mul","y","0"],["add","y","25"],["mul","y","x"],["add","y","1"],["mul","z","y"],["mul","y","0"],["add","y","w"],["add","y","10"],["mul","y","x"],["add","z","y"]]]

test :: IO ()
test = hspec $ do
  describe "Check my guesses" $ do
    it "up to 1 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 1 (chooseInt (1,9))) sameAnswer
    it "up to 2 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 2 (chooseInt (1,9))) sameAnswer
    it "up to 3 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 3 (chooseInt (1,9))) sameAnswer
    it "up to 4 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 4 (chooseInt (1,9))) sameAnswer
    it "up to 5 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 5 (chooseInt (1,9))) sameAnswer
    it "up to 6 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 6 (chooseInt (1,9))) sameAnswer
    it "up to 7 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 7 (chooseInt (1,9))) sameAnswer
    it "up to 8 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 8 (chooseInt (1,9))) sameAnswer
    it "up to 9 digits" $
      let sameAnswer :: [Int] -> Bool
          sameAnswer digits = compute digits sections == formula digits
      in property $ forAll (vectorOf 9 (chooseInt (1,9))) sameAnswer
