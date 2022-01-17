module Main where

import Prelude

import Effect.Console (log)
import Data.List as List
import Data.List (List, (:))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff

foreign import argv :: Array String

args :: List String
args = List.drop 2 $ List.fromFoldable argv

main :: Effect Unit
main = do
  pure unit
-- main = Aff.launchAff_ do
--   case args of
--     "help" : rest -> Aff.liftEffect $ log "help"
--     List.Nil ->  Aff.liftEffect $log "help"
--     _ -> do
--        Aff.liftEffect $log $ "Unknown arguments: " <> List.intercalate " " args
