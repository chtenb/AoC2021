module ReadLines where

import Prelude

import Control.Monad.List.Trans (ListT(..))
import Data.Identity (Identity(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Iterator (IteratorT, toList)

foreign import readLinesImpl :: String -> IteratorT Effect String

readLines :: String -> IteratorT Effect String
readLines filename = readLinesImpl filename

-- readLines' :: String -> Effect (ListT Identity String)
-- readLines' filename = readLinesImpl filename # toList # sequence

