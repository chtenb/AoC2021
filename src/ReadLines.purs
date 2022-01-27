module ReadLines where

import Prelude

import Data.List (List)
import Effect (Effect)
import Iterator (IteratorT, toList)

foreign import readLinesImpl :: String -> IteratorT Effect String

readLines :: String -> IteratorT Effect String
readLines filename = readLinesImpl filename

readLinesList :: String -> Effect (List String)
readLinesList filename = readLinesImpl filename # toList
