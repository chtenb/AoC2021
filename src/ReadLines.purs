module ReadLines where

import Prelude

import Effect (Effect)
import Iterator

foreign import readLinesImpl :: String -> (Iterator (Effect String))

readLines :: String -> Iterator (Effect String)
readLines filename = readLinesImpl filename

