module E1 where

import Prelude
import Utils (listOfMaybesToMaybeList, readFile)

import Data.Either (Either(..), note)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.String.Utils (lines)
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error, error)

main :: List String -> Effect Unit
main args = do
  case args of
    filename : List.Nil -> do
      eitherInts <- parseFile filename
      case eitherInts of
        Left err -> Console.error $ "Could not read or parse file: " <> show err
        Right ints -> Console.log $ show $ computeResult ints
    _ -> Console.error "provide filename"

parseFile :: String -> Effect (Either Error (List Int))
parseFile filename = do
  eitherText <- readFile filename
  pure $ eitherText >>= (\text -> parseText text # note (error "something wrong"))

parseText :: String -> Maybe (List Int)
parseText text = text # lines # List.fromFoldable # List.filter (not String.null) <#> Int.fromString # listOfMaybesToMaybeList

computeResult :: List Int -> Int
computeResult ints = diffInts ints # List.filter ((>) 0) # List.length

diffInts :: List Int -> List Int
diffInts List.Nil = List.Nil
diffInts (List.Cons first ints) = (List.foldr f { result : List.Nil, prev : first } ints).result
  where 
  f :: Int -> { result :: List Int, prev :: Int } -> { result :: List Int, prev :: Int }
  f int { result, prev } = { result : List.Cons (prev - int) result, prev : int }


