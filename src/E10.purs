module E10 where

import Prelude
import DebugUtils

import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (uncons, singleton)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Utils (listOfEithersToEitherList, readFile)

type UserErrorMessage = Either String
type LoggingErrorMessage = Either String

data SyntaxValidationResult = Ok | Round | Square | Curly | Angled | Unknown Char
type ParserState = { round :: Int, square :: Int, curly :: Int, angled :: Int, unparsed :: String }
initState :: String -> ParserState
initState string = {round:0, square:0, curly:0, angled:0, unparsed:string}

main :: List String -> Effect Unit
main args = do
  case args of
    filename : List.Nil -> do
      eitherStrings <- parseFile filename
      case eitherStrings of
        Left err -> Console.error $ "Could not read or parse file: " <> show err
        Right strings -> Console.log $ show $ computeScore strings
    _ -> Console.error "provide filename"

parseFile :: String -> Effect (Either Error (List String))
parseFile filename = do
  eitherText <- readFile filename
  pure $ eitherText <#> parseText

parseText :: String -> List String
parseText text = text # lines # List.fromFoldable # List.filter (not String.null)

computeScore :: List String -> Either String Int
computeScore strings = strings <#> computeLineScore # listOfEithersToEitherList <#> sum

computeLineScore :: String -> Either String Int
computeLineScore string = case validateSyntax $ initState string of
  Unknown x -> Left $ "Unknown char " <> singleton x
  Ok -> Right 0
  Round -> Right 3
  Square -> Right 57
  Curly -> Right 1197
  Angled -> Right 25137

validateSyntax :: ParserState -> SyntaxValidationResult
validateSyntax state = case uncons state.unparsed of
  Nothing -> Ok
  Just { head, tail } -> case head of
    '(' -> validateSyntax $ state { round = state.round + 1, unparsed = tail }
    ')' -> if state.round == 0 then Round else validateSyntax $ state { round = state.round - 1, unparsed = tail }
    '[' -> validateSyntax $ state { square = state.square + 1, unparsed = tail }
    ']' -> if state.square == 0 then Square else validateSyntax $ state { square = state.square - 1, unparsed = tail }
    '{' -> validateSyntax $ state { curly = state.curly + 1, unparsed = tail }
    '}' -> if state.curly == 0 then Curly else validateSyntax $ state { curly = state.curly - 1, unparsed = tail }
    '<' -> validateSyntax $ state { angled = state.angled + 1, unparsed = tail }
    '>' -> if state.angled == 0 then Angled else validateSyntax $ state { angled = state.angled - 1, unparsed = tail }
    x -> Unknown x

