module E3 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Eq ((/=))
import Data.Foldable (class Foldable, foldl)
import Data.Int as Int
import Data.Int (Radix(..))
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (toCharArray, fromCharArray)
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
        Right text -> case parseText text of
          Nothing -> Console.error $ "Could not parse file"
          Just parseResult -> case computeResult parseResult of
            Nothing -> Console.error $ "Something went wrong during computation"
            Just result -> Console.log $ show $ result
    _ -> Console.error "provide filename"

type DataPoint = String
type DiagnosticAcc = List { zeros :: Int, ones :: Int }
singleDiagnosticAcc :: DataPoint -> Maybe DiagnosticAcc 
singleDiagnosticAcc dataPoint = dataPoint # toCharArray # List.fromFoldable <#>
  (\char -> case char of 
    '0' -> Just {zeros:1,ones:0} 
    '1' -> Just {zeros:0,ones:1}
    _ -> Nothing)
  # listOfMaybesToMaybeList

parseText :: String -> Maybe DiagnosticAcc
parseText text = text # parseNonEmptyLines # accDiagnostics

parseNonEmptyLines :: String -> List String
parseNonEmptyLines text = text # lines # List.fromFoldable # List.filter (not String.null)

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

computeResult :: DiagnosticAcc -> Maybe Int
computeResult diagnosticAcc = do
  gamma <- gammaRate diagnosticAcc
  epsilon <- epsilonRate diagnosticAcc
  pure $ gamma * epsilon

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
