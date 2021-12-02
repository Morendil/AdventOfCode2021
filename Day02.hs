import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)
command = (,) <$> choice [string "forward", string "down", string "up"] <*> (string " " *> number)
commands = sepBy1 command (string "\n")

execute :: (Int, Int) -> (String, Int) -> (Int, Int)
execute (x, depth) ("forward", n) = (x+n, depth)
execute (x, depth) ("down", n) = (x, depth+n)
execute (x, depth) ("up", n) = (x, depth-n)
execute _ _ = error "Nope"

execute2 :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
execute2 (x, depth, aim) ("forward", n) = (x+n, depth+(n*aim), aim)
execute2 (x, depth, aim) ("down", n) = (x, depth, aim+n)
execute2 (x, depth, aim) ("up", n) = (x, depth, aim-n)
execute2 _ _ = error "Nope"


part1 :: [(String, Int)] -> Int
part1 commands = x * depth
    where (x, depth) = foldl execute (0, 0) commands

part2 :: [(String, Int)] -> Int
part2 commands = x * depth
    where (x, depth, aim) = foldl execute2 (0, 0, 0) commands

main = do
    cmds <- parseMaybe commands <$> readFile "day02.txt"
    print $ part1 $ fromJust cmds
    print $ part2 $ fromJust cmds

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
