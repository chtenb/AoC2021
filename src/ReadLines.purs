module ReadLines where

import Prelude

import Data.List (List)
import Data.List as List
import Effect (Effect)
import Iterator (IterationStep(..), IteratorT, getStep)

foreign import readLinesImpl :: String -> IteratorT Effect String

readLines :: String -> IteratorT Effect String
readLines filename = readLinesImpl filename

readLinesList :: String -> Effect (List String)
readLinesList filename = readLinesImpl filename # toList

toList :: IteratorT Effect String -> Effect (List String)
toList it =
  getStep it >>= \step -> case step of
    Done -> pure List.Nil
    Yield value rest -> toList rest >>= map pure (List.Cons value)
