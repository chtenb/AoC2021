module Test.Main where

import Prelude

import Effect (Effect)
import Test.Iterator as Test.Iterator

main :: Effect Unit
main = do
  Test.Iterator.main
