module E1 where

import Prelude

import Data.Either (Either(..), note)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.String as String
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Utils (listOfMaybesToMaybeList, readFile, writeFile)

main :: List String -> Effect Unit
main args = do
  case args of
    filename : List.Nil -> do
      eitherInts <- parseFile filename
      case eitherInts of
        Left err -> Console.error $ "Could not read or parse file: " <> show err
        Right ints ->  do
          _ <- writeFile "text.txt" $ List.foldl (\text int -> text <> show int <> "\n") "" ints
          if List.length ints /= 2000
          then Console.error $ "could not read all lines "
          else pure unit
          Console.log $ show increases <> " " <> show unchanged <> " " <> show decreases
          if increases + unchanged + decreases + 1 /= List.length ints
          then Console.error $ "bug: " <> (show $ increases + unchanged + decreases) <> " + 1 /= " <> (show $ List.length ints)
          else Console.log $ show $ increases
          where
          increases = countIncreases ints
          decreases = countDecreases ints
          unchanged = countUnchanged ints
    _ -> Console.error "provide filename"

parseFile :: String -> Effect (Either Error (List Int))
parseFile filename = do
  eitherText <- readFile filename
  pure $ eitherText >>= (\text -> parseText text # note (error "something wrong"))

parseText :: String -> Maybe (List Int)
parseText text = text # lines # List.fromFoldable # List.filter (not String.null) <#> Int.fromString # listOfMaybesToMaybeList

countIncreases :: List Int -> Int
countIncreases ints = diffInts ints # List.filter ((>) 0) # List.length

countDecreases :: List Int -> Int
countDecreases ints = diffInts ints # List.filter ((<) 0) # List.length

countUnchanged :: List Int -> Int
countUnchanged ints = diffInts ints # List.filter ((==) 0) # List.length

diffInts :: List Int -> List Int
diffInts List.Nil = List.Nil
diffInts (List.Cons first ints) = (List.foldr f { result : List.Nil, prev : first } ints).result
  where 
  f :: Int -> { result :: List Int, prev :: Int } -> { result :: List Int, prev :: Int }
  f int { result, prev } = { result : List.Cons (int - prev) result, prev : int }


