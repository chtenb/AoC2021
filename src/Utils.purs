module Utils where

import Prelude

import Control.Monad.Except (Except, except, runExcept)
import Data.Array as Array
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


-- Picks the first left if any
listOfEithersToEitherList :: forall a l . (Show a) => List (Either l a) -> Either l (List a)
listOfEithersToEitherList listOfEithers =
    List.reverse <$> List.foldl f (Right List.Nil) listOfEithers
      where
      f :: Either l (List a) -> Either l a -> Either l (List a)
      f eitherResultList either = case eitherResultList of
        Right resultList -> case either of
          Right elem -> Right $ List.Cons elem resultList
          Left l -> Left l
        l -> l

aggregateExceptList :: forall a e . (Show a) => List (Except e a) -> Except e (List a)
aggregateExceptList list = list <#> runExcept # listOfEithersToEitherList # except

aggregateExceptArray :: forall a e . (Show a) => Array (Except e a) -> Except e (Array a)
aggregateExceptArray list = list # List.fromFoldable # aggregateExceptList <#> Array.fromFoldable

repeat :: forall a . Int -> (a -> a) -> a -> a
repeat times f start = List.range 1 times # List.foldl (\a _ -> f a) start
