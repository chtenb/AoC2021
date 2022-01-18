module Utils where

import Prelude

import Data.Either (Either)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (Error, try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

readFile :: String -> Effect (Either Error String)
readFile filename = try $ readTextFile UTF8 filename

listOfMaybesToMaybeList :: forall a . (Show a) => List (Maybe a) -> Maybe (List a)
listOfMaybesToMaybeList listOfMaybes =
    List.foldr f (Just List.Nil) listOfMaybes
      where
      f maybeInt maybeResultList = case maybeResultList of
        Nothing -> Nothing
        Just resultList -> case maybeInt of
          Nothing -> Nothing
          Just int -> Just $ List.Cons int resultList

