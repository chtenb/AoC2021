module ReadLines where

import Prelude

import Effect (Effect)
import Iterator

foreign import readLinesImpl :: String -> (IteratorT Effect String)

readLines :: String -> IteratorT Effect String
readLines filename = readLinesImpl filename

