import Text.Printf
import Data.Char
import Data.List.HT
import Data.List.Split
import Data.Maybe
import Test.Hspec

data Packet = Literal {version :: Int, value :: Int} | Operator {version :: Int, typeId :: Int, packets :: [Packet]}
    deriving (Eq, Show)

versionSum :: Packet -> Int
versionSum Literal {version=version} = version
versionSum Operator {version=version, packets=packets} = version + sum (map versionSum packets)

evaluate :: Packet -> Int
evaluate Literal {value=value} = value
evaluate Operator {typeId=0, packets=packets} = sum (map evaluate packets)
evaluate Operator {typeId=1, packets=packets} = product (map evaluate packets)
evaluate Operator {typeId=2, packets=packets} = minimum (map evaluate packets)
evaluate Operator {typeId=3, packets=packets} = maximum (map evaluate packets)
evaluate Operator {typeId=5, packets=[a,b]} = if evaluate a > evaluate b then 1 else 0
evaluate Operator {typeId=6, packets=[a,b]} = if evaluate a < evaluate b then 1 else 0
evaluate Operator {typeId=7, packets=[a,b]} = if evaluate a == evaluate b then 1 else 0
evaluate packet = error $ "Wrong packet:" ++ show packet

toDecimal :: [Int] -> Int
toDecimal s = sum $ zipWith (\a b -> a * (2 ^ b)) (reverse s) [0..]

fromHex :: String -> String
fromHex = concatMap (printf "%04b" . digitToInt)

main = do
    packet <- parse <$> readFile "day16.txt"
    print $ versionSum packet
    print $ evaluate packet

parse :: String -> Packet
parse s = let (packet, rest, len) = parseDirect $ fromHex s in packet

parseDirect :: String -> (Packet, String, Int)
parseDirect s = (if typeId == 4 then parseLiteral version else parseOperator version typeId) $ drop 6 s
    where version = toDecimal $ map digitToInt $ take 3 s
          typeId = toDecimal $ map digitToInt $ take 3 $ drop 3 s

parseLiteral :: Int -> String -> (Packet, String, Int)
parseLiteral version s = (Literal {version=version, value=value}, rest, len+6)
    where (value, rest, len) = parseValue s

parseValue :: String -> (Int, String, Int)
parseValue s = (toDecimal $ concatMap (map digitToInt . tail) chunks, rest, 5*length chunks)
    where chunks = takeUntil ((==)'0'.head) $ chunksOf 5 s
          rest = drop (5*length chunks) s

parseOperator :: Int -> Int -> String -> (Packet, String, Int)
parseOperator version typeId s = (Operator {version=version, typeId=typeId, packets=packets}, rest, len+6)
    where (packets, rest, len) = if head s == '1' then byNum else byLen
            where byLen = parseByLen (num 15) version typeId (drop 16 s)
                  byNum = parseByNum (num 11) version typeId (drop 12 s)
                  num n = toDecimal $ map digitToInt $ take n $ tail s

parseByLen :: Int -> Int -> Int -> String -> ([Packet], String, Int)
parseByLen n version typeId s = get n ([],s)
    where get 0 (packets, rest) = (reverse packets, rest, n+16)
          get n (packets, rest) = let (packet, rest', len) = parseDirect rest in get (n-len) (packet:packets, rest')

parseByNum :: Int -> Int -> Int -> String -> ([Packet], String, Int)
parseByNum n version typeId s = get n 0 ([],s)
    where get 0 len (packets, rest) = (reverse packets, rest, len+12)
          get n len (packets, rest) = let (packet, rest', len') = parseDirect rest in get (n-1) (len+len') (packet:packets, rest')

test :: IO ()
test = hspec $ do
  describe "Parser" $ do
    it "succeeds on literal value" $
        parse "D2FE28" `shouldBe` Literal {version=6, value=2021}
    it "succeeds on 2-packet operator value" $
        parse "38006F45291200" `shouldBe` Operator {version=1, typeId=6, packets=[Literal {version=6, value=10},Literal {version=2, value=20}]}
    it "succeeds on 3-packet operator value" $
        parse "EE00D40C823060" `shouldBe` Operator {version=7, typeId=3, packets=[Literal {version=2, value=1},Literal {version=4, value=2},Literal {version=1, value=3}]}
    it "computes the correct sum, 12" $
        versionSum (parse "620080001611562C8802118E34") `shouldBe` 12
    it "computes the correct sum, 16" $
        versionSum (parse "8A004A801A8002F478") `shouldBe` 16
    it "computes the correct sum, 23" $
        versionSum (parse "C0015000016115A2E0802F182340") `shouldBe` 23
    it "computes the correct sum, 31" $
        versionSum (parse "A0016C880162017C3686B18A3D4780") `shouldBe` 31
