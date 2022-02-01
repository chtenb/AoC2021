module E11 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (Except, ExceptT, except, runExcept, runExceptT, throwError, withExceptT)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseDigit, parseEol, runParser, safeMany)
import Text.Parsing.StringParser (Parser)
import Utils (aggregateExceptArray, readFile)

main :: Effect Unit
main = do
  result <- runExceptT mainExceptEffect
  case result of
    Left l -> Console.log ("error: " <> show l)
    Right r -> Console.log ("success: " <> show r)

mainExceptEffect :: ExceptT String Effect Int
mainExceptEffect = do
  inputString <- lift readInput >>= except # withExceptT show
  input <- runInputParser inputString
  except $ runExcept $ mainExcept input

mainExcept :: EnergyMap -> Except String Int
mainExcept input = pure 0

-- PARSE INPUT

readInput :: Effect (Either Error String)
readInput = readFile "data/11-test.txt"

runInputParser :: String -> ExceptT String Effect EnergyMap
runInputParser inputString = runParser parseInput inputString # except # withExceptT show

parseInput :: Parser EnergyMap
parseInput = do
  lines <- safeMany parseLine
  pure $ Array.fromFoldable lines

parseLine :: Parser EnergyMapRow
parseLine = do
  line <- safeMany parseDigit
  _ <- parseEol
  pure $ EnergyMapRow $ Array.fromFoldable line

-- PRIMITIVES

type EnergyLevel = Int
newtype EnergyMapRow = EnergyMapRow (Array EnergyLevel)
type EnergyMap = Array EnergyMapRow
type EnergyMapCoord = { x :: Int, y :: Int}

getEnergy :: EnergyMap -> EnergyMapCoord -> Maybe EnergyLevel
getEnergy energyMap { x, y } = do
  EnergyMapRow row <- Array.index energyMap y
  Array.index row x

energyMapCoords :: EnergyMap -> Array EnergyMapCoord
energyMapCoords energyMap = Array.mapWithIndex (\y (EnergyMapRow row) -> Array.mapWithIndex (\x _ -> { x, y }) row) energyMap # Array.concat

-- GLUE

-- LOGIC

-- Array of neighbors
neighborCoords :: EnergyMap -> EnergyMapCoord -> Array EnergyMapCoord
neighborCoords energyMap coord =
  [ { x: coord.x, y: coord.y - 1}
  , { x: coord.x, y: coord.y + 1}
  , { x: coord.x - 1, y: coord.y}
  , { x: coord.x + 1, y: coord.y}
  ] # Array.filter (isValidCoord energyMap)

isValidCoord :: EnergyMap -> EnergyMapCoord -> Boolean
isValidCoord energyMap coord = case getEnergy energyMap coord of
  Nothing -> false
  Just _ -> true
