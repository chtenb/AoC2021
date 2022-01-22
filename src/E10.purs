module E10 where

import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Int (odd)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Utils (lines)
import DebugUtils (debug, debug_)
import Effect (Effect)
import Effect.Console as Console
import Utils (listOfEithersToEitherList, readFile)

type UserErrorMessage = Either String
type LoggingErrorMessage = Either String

type InputLine = List Char
data Bracket = Round | Square | Curly | Angled
derive instance Eq Bracket
data OrientedBracket = Open Bracket | Close Bracket
type ParserState = List Bracket
data SyntaxValidationResult = Ok | Incomplete ParserState | Corrupted Bracket | Unknown Char

parseNonEmptyLines :: String -> List InputLine
parseNonEmptyLines text = text # lines # List.fromFoldable # List.filter (not String.null) <#> toCharArray <#> List.fromFoldable

main :: List String -> Effect Unit
main args = do
  eitherText <- readFile "data/10.txt"
  case eitherText of
    Left err -> Console.error $ "Could not read file: " <> show err
    Right text -> case args of
      "1" : List.Nil -> case part1 text of
        Left err -> Console.error err
        Right score -> Console.log $ show score
      "2" : List.Nil -> case part2 text of
        Left err -> Console.error err
        Right score -> Console.log $ show score
      _  -> do Console.error "provide valid part"

part1 :: String -> Either String Int
part1 text = text # parseNonEmptyLines <#> parseInput <#> validationScore # listOfEithersToEitherList <#> sum

part2 :: String -> Either String Int
part2 text =
  text
  # parseNonEmptyLines
  <#> parseInput
  # List.mapMaybe
    case _ of
      Incomplete parseResult -> Just parseResult
      _ -> Nothing
  <#> flip autocompleteParserState 0
  # debug_
  # \results -> if odd $ List.length results then median results else Left "Not odd"

median :: List Int -> Either String Int
median ints = List.sort ints # flip List.index (List.length ints / 2) # note "Could not find median"

autocompleteParserState :: ParserState -> Int -> Int
autocompleteParserState List.Nil score = score
autocompleteParserState (bracket : parserState) score =
  5 * score + case bracket of
    Round -> 1
    Square -> 2
    Curly -> 3
    Angled -> 4
  # autocompleteParserState parserState

parseInput :: InputLine -> SyntaxValidationResult
parseInput = parseInputRec List.Nil
parseInputRec :: ParserState -> InputLine -> SyntaxValidationResult
parseInputRec List.Nil List.Nil = Ok
parseInputRec parserState List.Nil = Incomplete parserState
parseInputRec parserState (x:xs) =
  case parseBracket x of
    Left char -> Unknown char
    Right (Open bracket) -> parseInputRec (bracket : parserState) xs
    Right (Close bracket) ->
      case parserState of
        expectedBracket : rest | bracket == expectedBracket -> parseInputRec rest xs
        _ -> Corrupted bracket

parseBracket :: Char -> Either Char OrientedBracket
parseBracket char = case char of
  '(' -> Right $ Open Round
  '[' -> Right $ Open Square
  '{' -> Right $ Open Curly
  '<' -> Right $ Open Angled
  ')' -> Right $ Close Round
  ']' -> Right $ Close Square
  '}' -> Right $ Close Curly
  '>' -> Right $ Close Angled
  unknown -> Left unknown

validationScore :: SyntaxValidationResult -> Either String Int
validationScore = case _ of
  Unknown x -> Left $ "Unknown char " <> singleton x
  Incomplete _ -> Right 0
  Ok -> Right 0
  Corrupted Round -> Right 3
  Corrupted Square -> Right 57
  Corrupted Curly -> Right 1197
  Corrupted Angled -> Right 25137

