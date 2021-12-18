import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Text.Printf
import Data.Maybe
import Test.Hspec

data SnailFish = Num Int | Pair SnailFish SnailFish
    deriving (Eq, Show)

snailfish :: ReadP SnailFish
snailfish = number +++ pair
    where number = Num . read <$> many1 (satisfy isDigit)
          pair =  between (string "[") (string "]") (Pair <$> snailfish <*> (string "," *> snailfish))

parse :: ReadP a -> String ->  a
parse parser input =
    case reverse $ readP_to_S parser input of
        ((result, _):_) -> result
        _ -> error "No parse"

magnitude :: SnailFish -> Int
magnitude (Num n) = n
magnitude (Pair left right) = (3 * magnitude left) + (2 * magnitude right)

split :: SnailFish -> SnailFish
split s@(Num _) = s
split (Pair left right) = if needSplit left then Pair (doSplit left) right else Pair left (doSplit right)
    where needSplit (Num n) = n >= 10
          needSplit (Pair left right) = needSplit left || needSplit right
          doSplit (Num n) | n >= 10 = Pair (Num (n`div`2)) (Num (n-(n`div`2)))
          doSplit p = split p

type Explosion = Maybe ([Int], (Int, Int))

findExplosion :: Int -> [Int] -> SnailFish -> Explosion
findExplosion n path (Num _) = Nothing
findExplosion 4 path (Pair (Num left) (Num right)) = Just (reverse path, (left, right))
findExplosion n path (Pair left right)
  | isJust exploL = exploL
  | isJust exploR = exploR
  | otherwise = Nothing
    where exploL = findExplosion (n+1) (0:path) left
          exploR = findExplosion (n+1) (1:path) right

replace :: SnailFish -> [Int] -> SnailFish -> SnailFish
replace sf [] v = v
replace (Pair left right) (0:rest) v = Pair (replace left rest v) right
replace (Pair left right) (1:rest) v = Pair left (replace right rest v)
replace sf _ _ = sf

alter :: SnailFish -> [Int] -> Int -> Maybe SnailFish
alter (Num v) _ a = Just $ Num $ v+a
alter (Pair left right) (0:rest) v = (`Pair` right) <$> alter left rest v
alter (Pair left right) (1:rest) v = Pair left <$> alter right rest v
alter sf _ _ = Nothing

toDecimal :: [Int] -> Int
toDecimal bits = sum $ zipWith (\a b -> a * (2 ^ b)) (reverse bits) [0..]

toPath :: Int -> [Int]
toPath = map digitToInt . printf "%05b"

explode :: SnailFish -> SnailFish
explode snf = if isJust exp then remove $ changeLeft $ changeRight snf else snf
    where exp = findExplosion 0 [] snf
          (path, (left, right)) = fromJust exp
          remove sf = replace sf path (Num 0)
          lefts = tail $ takeWhile (>=0) (iterate pred $ toDecimal (path++[0]))
          rights = tail $ takeWhile (<=31) (iterate succ $ toDecimal (path++[1]))
          changeLeft sf = head $ mapMaybe (\np -> alter sf (toPath np) left) lefts ++ [sf]
          changeRight sf = head $ mapMaybe (\np -> alter sf (toPath np) right) rights ++ [sf]

reduce :: SnailFish -> SnailFish
reduce = converge (split . converge explode)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

add :: SnailFish -> SnailFish -> SnailFish
add s1 s2 = reduce $ Pair s1 s2

addList :: [String] -> SnailFish
addList = foldl1 add . map (parse snailfish)

sleft (Pair left right) = left
sright (Pair left right) = right

main = do
    entries <- lines <$> readFile "day18.txt"
    print $ magnitude $ addList entries
    let fishes = map (parse snailfish) entries
        allMags = [magnitude $ add s1 s2 | s1 <- fishes, s2 <- fishes, s1 /= s2]
    print $ maximum allMags

test :: IO ()
test = hspec $ do
  describe "Magnitude:" $ do
    it "gets the right answer, 129" $
      magnitude (parse snailfish "[[9,1],[1,9]]") `shouldBe` 129
    it "gets the right answer, 143" $
      magnitude (parse snailfish "[[1,2],[[3,4],5]]") `shouldBe` 143
    it "gets the right answer, 1384" $
      magnitude (parse snailfish "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") `shouldBe` 1384
    it "gets the right answer, 3488" $
      magnitude (parse snailfish "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") `shouldBe` 3488
  describe "Splitting:" $ do
    it "splits correctly, once" $
      split (parse snailfish "[[[[0,7],4],[15,[0,13]]],[1,1]]") `shouldBe` parse snailfish "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
    it "splits correctly, the other" $
      split (parse snailfish "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]") `shouldBe` parse snailfish "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
  describe "Exploding:" $ do
    it "explodes correctly, no left" $
      explode (parse snailfish "[[[[[9,8],1],2],3],4]") `shouldBe` parse snailfish "[[[[0,9],2],3],4]"
    it "explodes correctly, no right" $
      explode (parse snailfish "[7,[6,[5,[4,[3,2]]]]]") `shouldBe` parse snailfish "[7,[6,[5,[7,0]]]]"
    it "explodes correctly, left and right" $
      explode (parse snailfish "[[6,[5,[4,[3,2]]]],1]") `shouldBe` parse snailfish "[[6,[5,[7,0]]],3]"
    it "explodes correctly, but only once" $
      explode (parse snailfish "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") `shouldBe` parse snailfish "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
    it "explodes correctly, completing the job" $
      explode (parse snailfish "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]") `shouldBe` parse snailfish "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
  describe "Reducing:" $ do
    it "reduces to the expected answer" $
      reduce (parse snailfish "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]") `shouldBe` parse snailfish "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
  describe "Adding:" $ do
    it "adds correctly up to 4" $
      addList ["[1,1]", "[2,2]", "[3,3]", "[4,4]"] `shouldBe` parse snailfish "[[[[1,1],[2,2]],[3,3]],[4,4]]"
    it "adds correctly up to 5" $
      addList ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"] `shouldBe` parse snailfish "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    it "adds correctly up to 6" $
      addList ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"] `shouldBe` parse snailfish "[[[[5,0],[7,4]],[5,5]],[6,6]]"
    it "adds correctly on the homework" $
      addList ["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]", "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"] `shouldBe` parse snailfish "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
