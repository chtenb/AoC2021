module E3 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Eq ((/=))
import Data.Foldable (class Foldable, foldl)
import Data.Int (Radix(..))
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits (charAt, fromCharArray, length, toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Utils (listOfMaybesToMaybeList, readFile)

main :: List String -> Effect Unit
main args = do
  case args of
    filename : List.Nil -> do
      eitherText <- readFile filename
      case eitherText of
        Left err -> Console.error $ "Could not read file: " <> show err
        Right text -> 
          let dataPoints = parseNonEmptyLines text
          in do
            case computePowerConsumption dataPoints of
              Nothing -> Console.error $ "Something went wrong with computing power consumption"
              Just result -> Console.log $ show $ result
            case computeLifeSupportRating dataPoints of
              Nothing -> Console.error $ "Something went wrong with computing life support rating"
              Just result -> Console.log $ show $ result
    _ -> Console.error "provide filename"

type DataPoint = String

parseNonEmptyLines :: String -> List DataPoint
parseNonEmptyLines text = text # lines # List.fromFoldable # List.filter (not String.null)

type BitCount = { zeros :: Int, ones :: Int }
emptyBitCount :: BitCount
emptyBitCount = {zeros: 0, ones:0}
type BitPosition = Int

countBits :: List DataPoint -> BitPosition -> Maybe BitCount
countBits dataPoints pos = List.foldl loop (Just emptyBitCount) dataPoints
  where 
  loop maybeBitCount dataPoint = case maybeBitCount of 
    Nothing -> Nothing
    Just bitCount -> case charAt pos dataPoint of
      Just '0' -> Just bitCount {zeros = bitCount.zeros + 1}
      Just '1' -> Just bitCount {ones = bitCount.ones + 1}
      _ -> Nothing

computeLifeSupportRating :: List DataPoint -> Maybe Int
computeLifeSupportRating dataPoints = do
  o <- oxygenGeneratorRating dataPoints
  s <- scrubberRating dataPoints
  pure $ o * s

oxygenGeneratorRating :: List DataPoint -> Maybe Int
oxygenGeneratorRating = dataPointFilter (\bitCount -> if bitCount.ones >= bitCount.zeros then '1' else '0')

scrubberRating :: List DataPoint -> Maybe Int
scrubberRating = dataPointFilter (\bitCount -> if bitCount.ones >= bitCount.zeros then '0' else '1')

dataPointFilter :: (BitCount -> Char) -> List DataPoint -> Maybe Int
dataPointFilter bitCriterium = dataPointFilterRec 0
  where
  dataPointFilterRec :: Int -> List DataPoint -> Maybe Int
  dataPointFilterRec _ (List.Nil) = Nothing
  dataPointFilterRec _ (List.Cons x List.Nil) = fromBinary x
  dataPointFilterRec bitPos dataPoints =
    dataPoints
    # List.filter (\dataPoint -> not (Maybe.isNothing bitCriteriumChar) && charAt bitPos dataPoint == bitCriteriumChar)
    # dataPointFilterRec (bitPos + 1)
    where
    bitCriteriumChar :: Maybe Char
    bitCriteriumChar = 
      case countBits dataPoints bitPos of
        Nothing -> Nothing
        Just bitCount -> Just $ bitCriterium bitCount

-- PART 1

computePowerConsumption :: List DataPoint -> Maybe Int
computePowerConsumption dataPoints = do
  diagnosticAcc <- accDiagnostics dataPoints
  gamma <- gammaRate diagnosticAcc
  epsilon <- epsilonRate diagnosticAcc
  pure $ gamma * epsilon

type DiagnosticAcc = List BitCount
singleDiagnosticAcc :: DataPoint -> Maybe DiagnosticAcc 
singleDiagnosticAcc dataPoint = dataPoint # toCharArray # List.fromFoldable <#>
  (\char -> case char of 
    '0' -> Just {zeros:1,ones:0} 
    '1' -> Just {zeros:0,ones:1}
    _ -> Nothing)
  # listOfMaybesToMaybeList

accDiagnostics :: List DataPoint -> Maybe DiagnosticAcc
accDiagnostics List.Nil = Nothing
accDiagnostics (firstDataPoint:xs) = foldl addDataPoint (singleDiagnosticAcc firstDataPoint) xs

addDataPoint :: Maybe DiagnosticAcc -> DataPoint -> Maybe DiagnosticAcc
addDataPoint maybeDiagnosticAcc dataPoint = case maybeDiagnosticAcc of
  Nothing -> Nothing
  Just diagnosticAcc -> let chars = dataPoint # toCharArray # List.fromFoldable in
    if List.length chars /= List.length diagnosticAcc
    then Nothing
    else List.zip chars diagnosticAcc <#>
      (\(Tuple char bitPositionAcc) -> case char of 
        '0' -> Just bitPositionAcc { zeros = bitPositionAcc.zeros + 1 }
        '1' -> Just bitPositionAcc { ones = bitPositionAcc.ones + 1 }
        _ -> Nothing)
      # listOfMaybesToMaybeList

gammaRate :: DiagnosticAcc -> Maybe Int
gammaRate diagnosticAcc = diagnosticAcc
  <#> (\{zeros, ones} -> if zeros <= ones then '1' else '0')
  # Array.fromFoldable
  # fromCharArray
  # fromBinary

epsilonRate :: DiagnosticAcc -> Maybe Int
epsilonRate diagnosticAcc = diagnosticAcc
  <#> (\{zeros, ones} -> if zeros <= ones then '0' else '1')
  # Array.fromFoldable
  # fromCharArray
  # fromBinary

fromBinary :: String -> Maybe Int
fromBinary = Int.fromStringAs Int.binary
