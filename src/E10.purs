module E10 where

import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Utils (readFile)

type SyntaxState = { round :: Int, square :: Int, curly :: Int, angled :: Int }

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

computeScore :: List String -> Int
computeScore strings = sum $ computeLineScore <$> strings

computeLineScore :: String -> Int
computeLineScore string = 0



