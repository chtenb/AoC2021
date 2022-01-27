module Test.TestE4 where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Test.QuickCheck (Result(..), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)


main :: Effect Unit
main = do
  Console.log "🍝"

