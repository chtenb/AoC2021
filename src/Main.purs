module Main where

import Prelude
import E1 as E1
import E10 as E10

import Data.List (List, (:))
import Data.List as List
import Effect (Effect)
import Effect.Console as Console
foreign import argv :: Array String

args :: List String
args = List.drop 2 $ List.fromFoldable argv


main :: Effect Unit
main = do
  case args of
    "1" : rest -> E1.main rest
    "10" : rest -> E10.main rest
    _ -> Console.error "provide exercise"
