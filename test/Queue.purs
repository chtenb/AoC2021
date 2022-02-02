module Test.Queue where

import Prelude
import Queue
import Test.QuickCheck
import Test.Utils

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Debug (spy)
import Effect (Effect)
import Effect.Console as Console


main :: Effect Unit
main = do
  assertSuccess testPopEmpty
  testPop
  pure unit


testPopEmpty :: Result
testPopEmpty = assertEquals true (isNothing (pop empty))

testPop :: Effect Unit
testPop = do
  let q = empty # push 1 # push 2 # push 3 # drop1 # drop1 # push 11 # push 12 # push 13 # drop1 # drop1 # drop1 # push 21 # push 22 # push 23
  _ <- pure (spy "queue" q)
  pure unit
