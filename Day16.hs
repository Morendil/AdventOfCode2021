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

bITS :: ReadP Packet
bITS = literalPacket <++ operatorPacket

literalPacket :: ReadP Packet
literalPacket = do
    version <- bits 3
    string "100"
    literal <- literalValue
    return Literal {version=version, value=literal}

operatorPacket :: ReadP Packet
operatorPacket = do
    version <- bits 3
    typeId <- bits 3
    length <- (string "0" *> bits 15) <++ (string "1" *> bits 11)
    packets <- many1 bITS    
    return Operator {version=version, typeId=typeId, packets=packets}

bits :: Int -> ReadP Int
bits n = toDecimal . map digitToInt <$> count n (satisfy isDigit)

toDecimal :: [Int] -> Int
toDecimal s = sum $ zipWith (\a b -> a * (2 ^ b)) (reverse s) [0..]

literalValue :: ReadP Int
literalValue = toDecimal . map digitToInt <$> collect
    where one = count 4 (satisfy isDigit)
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

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

test :: IO ()
test = hspec $ do
  describe "Parser" $ do
    it "succeeds on literal value" $
        fromJust (parseMaybe bITS $ fromHex "D2FE28") `shouldBe` Literal {version=6, value=2021}
    it "succeeds on 2-packet operator value" $
        fromJust (parseMaybe bITS $ fromHex "38006F45291200") `shouldBe` Operator {version=1, typeId=6, packets=[Literal {version=6, value=10},Literal {version=2, value=20}]}
    it "succeeds on 3-packet operator value" $
        fromJust (parseMaybe bITS $ fromHex "EE00D40C823060") `shouldBe` Operator {version=7, typeId=3, packets=[Literal {version=2, value=1},Literal {version=4, value=2},Literal {version=1, value=3}]}
    it "computes the correct sum, 16" $
        versionSum (fromJust (parseMaybe bITS $ fromHex "620080001611562C8802118E34")) `shouldBe` 12
    it "computes the correct sum, 16" $
        versionSum (fromJust (parseMaybe bITS $ fromHex "8A004A801A8002F478")) `shouldBe` 16
    it "computes the correct sum, 23" $
        versionSum (fromJust (parseMaybe bITS $ fromHex "C0015000016115A2E0802F182340")) `shouldBe` 23
    it "computes the correct sum, 31" $
        versionSum (fromJust (parseMaybe bITS $ fromHex "A0016C880162017C3686B18A3D4780")) `shouldBe` 31
