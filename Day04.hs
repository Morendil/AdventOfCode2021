import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Maybe

type Board = [[Int]]
type Called = [Int]

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)
numbers :: ReadP [Int]
numbers = sepBy1 number (string ",")
row :: ReadP [Int]
row = sepBy1 (do; d1 <- get; d2 <- get; return $ read [d1,d2]) (string " ")
board :: ReadP [[Int]]
board = sepBy1 row (string "\n")

bingo :: ReadP ([Int], [Board])
bingo = do
    draws <- numbers
    string "\n\n"
    boards <- sepBy1 board (string "\n\n")
    return (draws, boards)

wins :: Called -> Board -> Bool
wins called board = winByRow || winByCol
    where full row = all (`elem` called) row
          winByRow = any full board
          winByCol = any full (transpose board)

score :: Called -> Board -> Int
score called board = sum (concat board \\ called) * last called

winner :: [Board] -> Called -> Maybe Int
winner boards called = if null winning then Nothing else Just $ score called (head winning)
    where winning = filter (wins called) boards

main = do
    (called, boards) <- parse bingo <$> readFile "day04.txt"
    let winnerScore = head $ take 1 $ mapMaybe (winner boards) (tail $ inits called)
    -- part1
    print winnerScore
    -- part2
    let lastCall = last $ takeWhile (\called -> not (all (wins called) boards)) (tail $ inits called)
        nextCall = take (length lastCall + 1) called
        lastWinner = head $ filter (not.wins lastCall) boards
    print $ score nextCall lastWinner

parse :: ReadP a -> String ->  a
parse parser input =
    case reverse $ readP_to_S parser input of
        ((result, _):_) -> result
        _ -> error "No parse"
