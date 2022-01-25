module ReadLines where

import Effect (Effect)
import Iterator (IteratorT)

foreign import readLinesImpl :: String -> IteratorT Effect String

readLines :: String -> IteratorT Effect String
readLines filename = readLinesImpl filename

