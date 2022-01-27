module Main where

import Prelude
import E1 as E1
import E2 as E2
import E3 as E3
import E4 as E4
import E5 as E5
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
    "2" : _ -> E2.main
    "3" : rest -> E3.main rest
    "4" : _ -> E4.main
    "5" : _ -> E5.main
    "10" : rest -> E10.main rest
    _ -> Console.error "provide valid exercise"
