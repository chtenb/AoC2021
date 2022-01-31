module E8 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (ExceptT, except, runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (foldl, sum)
import Data.Int (fromString) as Int
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap, class Newtype)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import DebugUtils (debug_)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseEol, runParser, safeMany)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (regex, string)
import Text.Parsing.StringParser.Combinators (sepBy, sepEndBy)
import Utils (listOfEithersToEitherList, readFile)

readInput :: Effect (Either Error String)
readInput = readFile "data/8.txt"

main :: Effect Unit
main = do
  result <- runExceptT main' 
  case result of
    Left l -> Console.log ("error: " <> show l)
    Right r -> Console.log ("success: " <> show r)

main' :: ExceptT String Effect Int
main' = do
  inputString <- lift readInput >>= except # withExceptT show
  inputs <- runParser (safeMany parseInput) inputString # except # withExceptT show
  lift $ Console.log $ show $ correctFrequencyMap
  decodedNumbers <- inputs <#> compute # listOfEithersToEitherList # except
  pure $ sum decodedNumbers

compute :: Input -> Either String Int
compute input = do
  freqMap <- pure $ findFrequencyMap input.uniqueOccurrences # debug_
  decodedDigits <- decodeOutputValues input freqMap
  Int.fromString decodedDigits # (note $ "could not parse " <> show decodedDigits)

decodeOutputValues :: Input -> FrequencyMap -> Either String String
decodeOutputValues { outputValues } freqMap = do
  charList <- outputValues <#> (\outputValue -> String.toCharArray outputValue <#> decodeChar freqMap # List.fromFoldable # listOfEithersToEitherList <#> Array.fromFoldable >>= toDisplayDigit) # listOfEithersToEitherList
  Right $ charList # Array.fromFoldable # String.fromCharArray

decodeOutputValue :: SignalPattern -> FrequencyMap -> Either String Char
decodeOutputValue outputValue freqMap = 
  String.toCharArray outputValue <#> decodeChar freqMap # List.fromFoldable # listOfEithersToEitherList <#> Array.fromFoldable >>= toDisplayDigit

decodeChar :: FrequencyMap -> Char -> Either String Char
decodeChar freqMap char = case Map.lookup char freqMap of
  Just (Frequency 8) -> Right 'a'
  Just (Frequency 10) -> Right 'b'
  Just (Frequency 12) -> Right 'c'
  Just (Frequency 11) -> Right 'd'
  Just (Frequency 4) -> Right 'e'
  Just (Frequency 13) -> Right 'f'
  Just (Frequency 7) -> Right 'g'
  Just (Frequency f) -> Left $ "Unexpected frequency " <> show f <> " for character " <> show char
  Nothing -> Left $ "could not decode " <> String.fromCharArray [char]

toDisplayDigit :: Array Char -> Either String Char
toDisplayDigit chars =
  List.findMap 
    (\{pattern, displayDigit} -> 
      if pattern == inputPattern then Just displayDigit else Nothing
    ) correctUniqueSignalPatterns
  # note ("Could not convert to display digit: " <> show chars)
  where
  inputPattern = chars # Array.sort # String.fromCharArray

correctFrequencyMap :: FrequencyMap
correctFrequencyMap = findFrequencyMap (correctUniqueSignalPatterns <#> _.pattern)

findByFrequency :: FrequencyMap -> Frequency -> Maybe Char
findByFrequency freqMap freq = toList freqMap # List.find (\(Tuple _ frequency) -> frequency == freq) <#> \(Tuple char _) -> char

-- sum segment frequencies of all unique signal patterns, and add the segments from signal pattern four another 3 times
findFrequencyMap :: List SignalPattern -> FrequencyMap
findFrequencyMap uniqueOccurrences = foldl addFrequencyMap emptyFrequencyMap occurenceMaps
  where
  occurenceMaps = occurences <#> fromSignalPattern
  occurences = four : four : four : four : uniqueOccurrences 
  byLength = findPatternsByLength uniqueOccurrences
  four = byLength.four

fromSignalPattern :: SignalPattern -> FrequencyMap
fromSignalPattern pattern = String.toCharArray pattern # foldl go emptyFrequencyMap
  where
  go freqmap char = fromChar char # SemigroupMap # ((<>) (SemigroupMap freqmap)) # unwrap

fromChar :: Char -> FrequencyMap
fromChar char = emptyFrequencyMap # Map.insert char (Frequency 1)


-- PRIMITIVES

newtype Frequency = Frequency Int
derive instance Newtype Frequency _
derive instance Eq Frequency
derive instance Ord Frequency
instance Semigroup Frequency where
  append (Frequency a) (Frequency b) = Frequency (a+b)
instance Show Frequency where
  show (Frequency a) = show a

type SignalPattern = String
type CorrectSignalPattern = {pattern::SignalPattern, displayDigit::Char} 
correctUniqueSignalPatterns :: List CorrectSignalPattern
correctUniqueSignalPatterns = 
  { pattern:"abcefg", displayDigit:'0' }
  : { pattern:"cf" , displayDigit: '1' }
  : { pattern:"acdeg" , displayDigit: '2' }
  : { pattern:"acdfg" , displayDigit: '3' }
  : { pattern:"bcdf" , displayDigit: '4' }
  : { pattern:"abdfg" , displayDigit: '5' }
  : { pattern:"abdefg" , displayDigit: '6' }
  : { pattern:"acf" , displayDigit: '7' }
  : { pattern:"abcdefg" , displayDigit: '8' }
  : { pattern:"abcdfg" , displayDigit: '9' }
  : List.Nil

type FrequencyMap = Map Char Frequency
emptyFrequencyMap :: FrequencyMap
emptyFrequencyMap = 'a':'b':'c':'d':'e':'f':'g':List.Nil <#> (\c -> Tuple c (Frequency 0)) # Map.fromFoldable
addFrequencyMap :: FrequencyMap -> FrequencyMap -> FrequencyMap
addFrequencyMap a b = SemigroupMap a <> SemigroupMap b # unwrap
toList :: FrequencyMap -> List (Tuple Char Frequency)
toList = Map.toUnfoldable

type Input = {uniqueOccurrences::List SignalPattern, outputValues::List SignalPattern}

type PatternsByLength =
  { two::SignalPattern
  , three::SignalPattern
  , four::SignalPattern
  , five::List SignalPattern
  , six::List SignalPattern
  }

findPatternsByLength :: List SignalPattern -> PatternsByLength
findPatternsByLength uniqueOccurrences =
  { two : List.find (hasLength 2) uniqueOccurrences # unsafePartial fromJust
  , three : List.find (hasLength 3) uniqueOccurrences # unsafePartial fromJust
  , four : List.find (hasLength 4) uniqueOccurrences # unsafePartial fromJust
  , five : List.filter (hasLength 5) uniqueOccurrences
  , six : List.filter (hasLength 6) uniqueOccurrences
  }
  where
  hasLength l = \s -> String.length s == l

-- PARSER

parseInput :: Parser Input
parseInput = do
  uniqueOccurrences <- sepEndBy (regex """\w+""") (string " ")
  _ <- string "| "
  outputValues <- sepBy (regex """\w+""") (string " ")
  _ <- parseEol
  pure $ {uniqueOccurrences, outputValues}
