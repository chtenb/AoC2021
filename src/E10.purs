module E10 where

import DebugUtils
import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (singleton, toCharArray, uncons)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Utils (listOfEithersToEitherList, readFile)

type UserErrorMessage = Either String
type LoggingErrorMessage = Either String

type InputLine = List Char
data Bracket = Round | Square | Curly | Angled
derive instance Eq Bracket
data OrientedBracket = Open Bracket | Close Bracket
type ParserState = List Bracket
data SyntaxValidationResult = Ok | Incomplete | Corrupted Bracket | Unknown Char

parseNonEmptyLines :: String -> List InputLine
parseNonEmptyLines text = text # lines # List.fromFoldable # List.filter (not String.null) <#> toCharArray <#> List.fromFoldable

main :: List String -> Effect Unit
main args = do
  case args of
    filename : List.Nil -> do
      eitherText <- readFile filename
      case eitherText of
        Left err -> Console.error $ "Could not read file: " <> show err
        Right text -> case wholeThing text of
          Left err -> Console.error err
          Right score -> Console.log $ show score
    _ -> Console.error "provide filename"

wholeThing :: String -> Either String Int
wholeThing text = text # parseNonEmptyLines <#> parseInput <#> validationScore # listOfEithersToEitherList <#> sum

parseInput :: InputLine -> SyntaxValidationResult
parseInput = parseInputRec List.Nil
parseInputRec :: ParserState -> InputLine -> SyntaxValidationResult
parseInputRec _ List.Nil = Ok
parseInputRec parserState (List.Cons x xs) =
  case parseBracket x of
    Left char -> Unknown char
    Right (Open bracket) -> parseInputRec (List.Cons bracket parserState) xs
    Right (Close bracket) ->
      case parserState of
        List.Cons expectedBracket rest | bracket == expectedBracket -> parseInputRec rest xs
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
  char -> Left char

validationScore :: SyntaxValidationResult -> Either String Int
validationScore = case _ of
  Unknown x -> Left $ "Unknown char " <> singleton x
  Incomplete -> Right 0
  Ok -> Right 0
  Corrupted Round -> Right 3
  Corrupted Square -> Right 57
  Corrupted Curly -> Right 1197
  Corrupted Angled -> Right 25137

