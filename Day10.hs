import Test.Hspec
import Test.QuickCheck.Property
import Data.Maybe
import Data.List

data Parse = Success | Incomplete String | Error Char
    deriving (Eq, Show)

closing :: Char -> Char
closing '(' = ')'
closing '{' = '}'
closing '[' = ']'
closing '<' = '>'
closing _ = error "Nope"

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = error "Nope"

autoScore :: String -> Int
autoScore string = sum $ zipWith (\a b -> a * (5 ^ b)) (reverse values) [0..]
    where table = [(')',1), (']',2), ('}',3), ('>',4)]
          value c = fromJust $ lookup c table
          values = map value string

parse :: String -> Parse
parse string = doParse string []

doParse :: String -> [Char] -> Parse
doParse "" [] = Success
doParse "" stack = Incomplete stack
doParse (c:rest) expect | c `elem` ")>]}" = if not (null expect) && c == head expect then doParse rest (tail expect) else Error c
doParse (c:rest) expect = doParse rest (closing c:expect)

scoreErrors :: Parse -> Maybe Int
scoreErrors (Error c) = Just $ score c
scoreErrors _ = Nothing

scoreIncomplete :: Parse -> Maybe Int
scoreIncomplete (Incomplete stack) = Just $ autoScore stack
scoreIncomplete _ = Nothing

main = do
    entries <- lines <$> readFile "day10.txt"
    -- part1
    print $ sum $ mapMaybe (scoreErrors . parse) entries
    -- part2
    let scores = sort $ mapMaybe (scoreIncomplete . parse) entries
        half = length scores `div` 2
        middle = scores !! half
    print middle

test :: IO ()
test = hspec $ do
  describe "Parser" $ do
    it "succeeds on empty string" $
        parse "" `shouldBe` Success
    it "fails on lone close" $
        parse ")" `shouldBe` Error ')'
    it "succeeds on balanced paren" $
        parse "()" `shouldBe` Success
    it "succeeds on sets of balanced paren" $
        parse "()<>" `shouldBe` Success
    it "succeeds on nested symbols" $
        parse "(<>)" `shouldBe` Success
    it "fails on lone close, nested" $
        parse "{[])" `shouldBe` Error ')'
    it "fails on lone close, sequential" $
        parse "[]<)" `shouldBe` Error ')'
    it "returns the expected result on a longer string" $
        parse "{([(<{}[<>[]}>{[]{[(<()>" `shouldBe` Error '}'
    it "returns the expected result on a longer string" $
        parse "[{[{({}]{}}([{[{{{}}([]" `shouldBe` Error ']'
    it "computes autocompletion for a small string" $
        parse "(" `shouldBe` Incomplete ")"
    it "computes autocompletion for a longer string" $
        parse "([{}" `shouldBe` Incomplete "])"
    it "computes autocompletion score correctly" $
        autoScore "}}]])})]" `shouldBe` 288957

