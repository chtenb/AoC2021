{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Main where

import Control.Monad (join)
import Data.Array (Array, array, bounds, (!))
import Data.Bifunctor (Bifunctor (first))
import Data.Char (Char, ord)
import Data.Foldable (Foldable (foldMap), all)
import qualified Data.List as List
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PSQueue
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace, traceM, traceShow)
import Text.Parsec (Parsec, char, count, digit, label, lookAhead, many, manyTill, parse, try, (<?>), (<|>))
import Text.Parsec.Char (string)
import Prelude

main :: IO ()
main = do
  putStrLn "Hello!"
  input <- readInput
  case run input of
    Left msg -> putStrLn msg
    Right result -> do
      putStrLn "Success!!"
      putStrLn $ "Result is:" ++ show result

readInput :: IO String
readInput = readFile "data/16.txt"

run :: String -> Except Int
run input = do
  binary <- transmissionToBinary input
  bitPacket <- binaryToBitPacket (trace binary binary)
  pure $ evalBitPacket bitPacket

addVersions :: [BitPacket] -> Int
addVersions [] = 0
addVersions (BitPacket version content : xs) = version + go content + addVersions xs
  where
    go (LiteralValue _) = 0
    go (Operator _ subPackets) = addVersions subPackets

type Except = Either String

type Version = Int

data BitPacket = BitPacket Version BitPacketContent
  deriving (Show)

data BitPacketContent = LiteralValue Int | Operator OperatorType [BitPacket]
  deriving (Show)

data OperatorType = Sum | Product | Min | Max | Greater | Less | Equal
  deriving (Show)

evalBitPacket :: BitPacket -> Int
evalBitPacket (BitPacket _ content) = evalContent content
  where
    evalContent (LiteralValue i) = i
    evalContent (Operator operatorType subPackets) = evalOperator operatorType (evalBitPacket <$> subPackets)
    evalOperator :: OperatorType -> [Int] -> Int
    evalOperator Sum xs = sum xs
    evalOperator Product xs = product xs
    evalOperator Min xs = minimum xs
    evalOperator Max xs = maximum xs
    evalOperator Greater [x, y] = if x > y then 1 else 0
    evalOperator Less [x, y] = if x < y then 1 else 0
    evalOperator Equal [x, y] = if x == y then 1 else 0
    evalOperator _ _ = error "invalid stuff"

tryCount n = try . count n

binaryToBitPacket :: String -> Except BitPacket
binaryToBitPacket binInput = first show $ parse binToBitPacket "" binInput

binaryToBitPackets :: String -> Except [BitPacket]
binaryToBitPackets binInput = first show $ parse (many binToBitPacket) "" binInput

binToBitPacket :: Parsec String () BitPacket
binToBitPacket = do
  version <- parseVersion
  typeId <- parseTypeId
  content <- case typeId of
    4 -> parseLiteral
    operatorId -> parseOperator operatorId
  -- x -> fail $ "Invalid type id: " ++ show x
  pure $ BitPacket version content

parseVersion :: Parsec String () Int
parseVersion = binToInt <$> tryCount 3 digit <?> "3 digits for version"

parseTypeId :: Parsec String () Int
parseTypeId = binToInt <$> tryCount 3 digit <?> "3 digits for type id"

parseOperator :: Int -> Parsec String () BitPacketContent
parseOperator operatorId = do
  lengthType <- parseLengthType
  subPackets <- case lengthType of
    0 -> parsePacketsOfLength
    1 -> parseNPackets
    _ -> fail "Invalid length id"
  pure $ Operator (intToOperatorType operatorId) subPackets
  where
    parseLengthType :: Parsec String () Int
    parseLengthType = binToInt . (: []) <$> (char '0' <|> char '1')

    intToOperatorType :: Int -> OperatorType
    intToOperatorType 0 = Sum
    intToOperatorType 1 = Product
    intToOperatorType 2 = Min
    intToOperatorType 3 = Max
    intToOperatorType 5 = Greater
    intToOperatorType 6 = Less
    intToOperatorType 7 = Equal
    intToOperatorType _ = error "Invalid operator id"

    parsePacketsOfLength :: Parsec String () [BitPacket]
    parsePacketsOfLength = do
      traceM "parsingPacketsOfLength"
      n <- binToInt <$> (tryCount 15 digit <?> "15 digits for length")
      payload <- tryCount n digit <?> show n ++ " bits as payload"
      case binaryToBitPackets payload of
        Left msg -> fail msg
        Right subPackets -> pure subPackets

    parseNPackets :: Parsec String () [BitPacket]
    parseNPackets = do
      traceM "parsingNPackets"
      n <- binToInt <$> (tryCount 11 digit <?> "11 digits for length")
      tryCount n binToBitPacket <?> show n ++ " packets as payload"

parseLiteral :: Parsec String () BitPacketContent
parseLiteral = flip label "parseLiteral" $ do
  traceM "parsingLiteral"
  groups <- many parseLiteralGroup
  endGroup <- parseLiteralGroupEnd
  let literal = join $ groups ++ [endGroup]
  let value = binToInt literal
  traceM $ "parsed literal groups " ++ literal
  pure $ LiteralValue $ traceShow value value
  where
    parseLiteralGroup :: Parsec String () String
    parseLiteralGroup = try $ char '1' *> (tryCount 4 digit <?> "4 bits as literal group")

    parseLiteralGroupEnd :: Parsec String () String
    parseLiteralGroupEnd = try $ char '0' *> (tryCount 4 digit <?> "4 bits as final literal group")

binToInt :: String -> Int
binToInt input = go $ reverse input
  where
    go [] = 0
    go ('0' : xs) = 0 + 2 * go xs
    go ('1' : xs) = 1 + 2 * go xs
    go _ = error "Invalid binary string"

transmissionToBinary :: String -> Except String
transmissionToBinary hexInput = first show $ parse hexToBin "" hexInput
  where
    hexToBin :: Parsec String () String
    hexToBin = join <$> many hexCharToBin

hexCharToBin :: Parsec String () String
hexCharToBin =
  hexCharToBin0
    <|> hexCharToBin1
    <|> hexCharToBin2
    <|> hexCharToBin3
    <|> hexCharToBin4
    <|> hexCharToBin5
    <|> hexCharToBin6
    <|> hexCharToBin7
    <|> hexCharToBin8
    <|> hexCharToBin9
    <|> hexCharToBinA
    <|> hexCharToBinB
    <|> hexCharToBinC
    <|> hexCharToBinD
    <|> hexCharToBinE
    <|> hexCharToBinF
  where
    hexCharToBin0 = "0000" <$ string "0"
    hexCharToBin1 = "0001" <$ string "1"
    hexCharToBin2 = "0010" <$ string "2"
    hexCharToBin3 = "0011" <$ string "3"
    hexCharToBin4 = "0100" <$ string "4"
    hexCharToBin5 = "0101" <$ string "5"
    hexCharToBin6 = "0110" <$ string "6"
    hexCharToBin7 = "0111" <$ string "7"
    hexCharToBin8 = "1000" <$ string "8"
    hexCharToBin9 = "1001" <$ string "9"
    hexCharToBinA = "1010" <$ string "A"
    hexCharToBinB = "1011" <$ string "B"
    hexCharToBinC = "1100" <$ string "C"
    hexCharToBinD = "1101" <$ string "D"
    hexCharToBinE = "1110" <$ string "E"
    hexCharToBinF = "1111" <$ string "F"
