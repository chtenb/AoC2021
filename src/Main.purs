module Main where

import Prelude
import E1 as E1
import E2 as E2
import E3 as E3
import E4 as E4
import E5 as E5
import E6 as E6
import E7 as E7
import E8 as E8
import E9 as E9
import E10 as E10
import E11 as E11
import E12 as E12
import E13 as E13
import E14 as E14

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
    "6" : _ -> E6.main
    "7" : _ -> E7.main
    "8" : _ -> E8.main
    "9" : _ -> E9.main
    "10" : rest -> E10.main rest
    "11" : _ -> E11.main
    "12" : _ -> E12.main
    "13" : _ -> E13.main
    "14" : _ -> E14.main
    _ -> Console.error "provide valid exercise"
