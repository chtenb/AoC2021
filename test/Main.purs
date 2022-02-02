module Test.Main where

import Prelude

import Effect (Effect)
import Test.Iterator as Test.Iterator
import Test.Queue as Test.Queue

main :: Effect Unit
main = do
  Test.Queue.main
