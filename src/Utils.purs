module Utils where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (Error, try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)


readFile :: String -> Effect (Either Error String)
readFile filename = try $ readTextFile UTF8 filename

writeFile :: String -> String -> Effect (Either Error Unit)
writeFile filename text = try $ writeTextFile UTF8 filename text

listOfMaybesToMaybeList :: forall a . (Show a) => List (Maybe a) -> Maybe (List a)
listOfMaybesToMaybeList listOfMaybes =
    List.foldr f (Just List.Nil) listOfMaybes
      where
      f maybeInt maybeResultList = case maybeResultList of
        Nothing -> Nothing
        Just resultList -> case maybeInt of
          Nothing -> Nothing
          Just int -> Just $ List.Cons int resultList


-- Picks the last left if any
listOfEithersToEitherList :: forall a l . (Show a) => List (Either l a) -> Either l (List a)
listOfEithersToEitherList listOfEithers =
    List.foldr f (Right List.Nil) listOfEithers
      where
      f :: Either l a -> Either l (List a) -> Either l (List a)
      f either eitherResultList = case eitherResultList of
        Right resultList -> case either of
          Right elem -> Right $ List.Cons elem resultList
          Left l -> Left l
        l -> l
