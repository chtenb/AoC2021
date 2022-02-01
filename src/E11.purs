module E11 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (Except, ExceptT, except, runExcept, runExceptT, withExceptT)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseDigit, parseEol, runParser, safeMany)
import Text.Parsing.StringParser (Parser)
import Utils (readFile, repeat)

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
  grid <- pure $ createGrid input
  except $ runExcept $ mainExcept grid

-- PARSE INPUT

readInput :: Effect (Either Error String)
readInput = readFile "data/11.txt"

runInputParser :: String -> ExceptT String Effect (List (List Int))
runInputParser inputString = runParser parseInput inputString # except # withExceptT show

parseInput :: Parser (List (List Int))
parseInput = do
  lines <- safeMany parseLine
  pure $ lines

parseLine :: Parser (List Int)
parseLine = do
  line <- safeMany parseDigit
  _ <- parseEol
  pure $ line

createGrid :: List (List Int) -> OctopusGrid
createGrid rows = mapWithIndex (\y row -> mapWithIndex (createOctopus y) row) rows # List.concat # Map.fromFoldable

createOctopus :: Int -> Int -> Int -> Tuple Coord Octopus
createOctopus y x digit = Tuple { x, y } { energy: digit, flashCount: 0, hashFlashedInCurrentStep: false }

-- PRIMITIVES

type Octopus = { energy :: Int, flashCount :: Int, hashFlashedInCurrentStep :: Boolean }
type OctopusGrid = Map Coord Octopus
type Coord = { x :: Int, y :: Int}

gridToList :: OctopusGrid -> List (Tuple Coord Octopus)
gridToList = Map.toUnfoldable

isValidCoord :: OctopusGrid -> Coord -> Boolean
isValidCoord grid coord = Map.lookup coord grid # isJust

-- GLUE

mainExcept :: OctopusGrid -> Except String Int
-- mainExcept grid = simulate100Steps grid # countFlashes # pure
mainExcept grid = pure $ simulateUntilAllFlashing grid

simulate100Steps :: OctopusGrid -> OctopusGrid
simulate100Steps = repeat 100 simulateStep

simulateStep :: OctopusGrid -> OctopusGrid
simulateStep = increaseLevelOfOctopuses >>> processFlashes >>> resetEnergiesAndUpdateFlashCounts

simulateUntilAllFlashing :: OctopusGrid -> Int
simulateUntilAllFlashing = go 1
  where
  go round grid = 
    let x = grid # increaseLevelOfOctopuses # processFlashes 
    in if areAllOctopusesFlashing x then round
    else go (round + 1) (resetEnergiesAndUpdateFlashCounts x)

-- LOGIC

increaseLevelOfOctopuses :: OctopusGrid -> OctopusGrid
increaseLevelOfOctopuses grid = grid <#> increaseLevelOfOctopus

increaseLevelOfOctopus :: Octopus -> Octopus
increaseLevelOfOctopus octopus = octopus { energy = octopus.energy + 1 }

increaseLevelOfOctopusAt :: OctopusGrid -> Coord -> OctopusGrid
increaseLevelOfOctopusAt grid coord = Map.update (increaseLevelOfOctopus >>> Just) coord grid

markOctopusAsFlashed :: OctopusGrid -> Coord -> OctopusGrid
markOctopusAsFlashed grid coord = Map.update (_ { hashFlashedInCurrentStep = true } >>> Just) coord grid

processFlashes :: OctopusGrid -> OctopusGrid
processFlashes grid =
  case findFlashingOctopus grid of
    Nothing -> grid
    Just coord -> processFlashes (processFlash grid coord)

findFlashingOctopus :: OctopusGrid -> Maybe Coord
findFlashingOctopus grid = 
  gridToList grid
    # List.find (\(Tuple _ octopus) -> octopus.energy > 9 && not octopus.hashFlashedInCurrentStep)
    <#> \(Tuple coord _) -> coord

processFlash :: OctopusGrid -> Coord -> OctopusGrid
processFlash grid coord = List.foldl increaseLevelOfOctopusAt grid neighbors # flip markOctopusAsFlashed coord
  where
  neighbors = neighborCoords grid coord

areAllOctopusesFlashing :: OctopusGrid -> Boolean
areAllOctopusesFlashing grid = Map.filter (_.hashFlashedInCurrentStep) grid # Map.size # eq 100

resetEnergiesAndUpdateFlashCounts :: OctopusGrid -> OctopusGrid
resetEnergiesAndUpdateFlashCounts grid =
  grid <#> \octopus -> 
    if 9 < octopus.energy 
    then octopus { energy = 0, flashCount = octopus.flashCount + 1, hashFlashedInCurrentStep = false }
    else octopus

countFlashes :: OctopusGrid -> Int
countFlashes grid = Map.values grid <#> _.flashCount # sum

-- Array of neighbors
neighborCoords :: OctopusGrid -> Coord -> List Coord
neighborCoords grid coord =
  [ { x: coord.x, y: coord.y - 1}
  , { x: coord.x, y: coord.y + 1}
  , { x: coord.x - 1, y: coord.y}
  , { x: coord.x + 1, y: coord.y}
  , { x: coord.x + 1, y: coord.y + 1}
  , { x: coord.x + 1, y: coord.y - 1}
  , { x: coord.x - 1, y: coord.y + 1}
  , { x: coord.x - 1, y: coord.y - 1}
  ] # List.fromFoldable # List.filter (isValidCoord grid)
