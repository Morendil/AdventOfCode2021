import Text.ParserCombinators.ReadP
import Text.Printf
import Data.Char
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Debug.Trace

data Packet = Literal {version :: Int, value :: Int} | Operator {version :: Int, typeId :: Int, packets :: [Packet]}
    deriving (Eq, Show)

versionSum :: Packet -> Int
versionSum Literal {version=version} = version
versionSum Operator {version=version, packets=packets} = version + sum (map versionSum packets)

bITS :: ReadP (Int, Packet)
bITS = literalPacket <++ operatorPacket

literalPacket :: ReadP (Int, Packet)
literalPacket = do
    version <- bits 3
    string "100"
    (len, literal) <- literalValue
    return (len+6, Literal {version=version, value=literal})

operatorPacket :: ReadP (Int, Packet)
operatorPacket = do
    version <- bits 3
    typeId <- bits 3
    lenPacket version typeId <++ numPacket version typeId

numPacket :: Int -> Int -> ReadP (Int, Packet)
numPacket version typeId = do
    string "1"
    num <- bits 11
    pairs <- count (traceShow ("num:"++show num) num) bITS
    let total = sum $ map fst $ traceShow ("num:"++show pairs) pairs
        packets = map snd pairs
    return (total+6, Operator {version=traceShow ("v:"++show version) version, typeId=typeId, packets=packets})

lenPacket :: Int -> Int -> ReadP (Int, Packet)
lenPacket version typeId = do
    string "0"
    length <- bits 15
    let doPackets :: Int -> [(Int, Packet)] -> ReadP [(Int, Packet)]
        doPackets n pairs = if n >= length then return $ reverse pairs
            else do
                pair@(len, packet) <- bITS
                doPackets (n+len) (pair:pairs)
    pairs <- doPackets 0 []
    let total = sum $ map fst $ traceShow ("len:"++show pairs) pairs
        packets = map snd pairs
    return (traceShowId $ total+6, Operator {version=version, typeId=typeId, packets=packets})

bits :: Int -> ReadP Int
bits n = toDecimal . map digitToInt <$> count n (satisfy isDigit)

toDecimal :: [Int] -> Int
toDecimal s = sum $ zipWith (\a b -> a * (2 ^ b)) (reverse s) [0..]

literalValue :: ReadP (Int, Int)
literalValue = consume <$> collect
    where consume s = (5 * (length s `div` 4), toDecimal $ map digitToInt (traceShow ("literal:"++s) s))
          one = count 4 (satisfy isDigit)
          collect = stop <++ continue
          stop = string "0" *> one
          continue = do
              string "1"
              high <- one
              rest <- collect
              return $ high ++ rest

fromHex :: String -> String
fromHex = concatMap (printf "%04b" . digitToInt)

main = do
    packet <- parseMaybe bITS . fromHex <$> readFile "day16.txt"
    print $ fromJust packet

parse :: String -> Packet
parse = snd . fromJust . parseMaybe bITS . fromHex

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

test :: IO ()
test = hspec $ do
  describe "Parser" $ do
    -- it "succeeds on literal value" $
    --     parse "D2FE28" `shouldBe` Literal {version=6, value=2021}
    -- it "succeeds on 2-packet operator value" $
    --     parse "38006F45291200" `shouldBe` Operator {version=1, typeId=6, packets=[Literal {version=6, value=10},Literal {version=2, value=20}]}
    -- it "succeeds on 3-packet operator value" $
    --     parse "EE00D40C823060" `shouldBe` Operator {version=7, typeId=3, packets=[Literal {version=2, value=1},Literal {version=4, value=2},Literal {version=1, value=3}]}
    -- it "computes the correct sum, 12" $
    --     versionSum (parse "620080001611562C8802118E34") `shouldBe` 12
    -- it "computes the correct sum, 16" $
    --     versionSum (parse "8A004A801A8002F478") `shouldBe` 16
    it "computes the correct sum, 23" $
        versionSum (parse "C0015000016115A2E0802F182340") `shouldBe` 23
    -- it "computes the correct sum, 31" $
    --     versionSum (parse "A0016C880162017C3686B18A3D4780") `shouldBe` 31
