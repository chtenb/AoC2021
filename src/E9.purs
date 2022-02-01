module E9 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (Except, ExceptT, except, runExcept, runExceptT, withExceptT)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseDigit, parseEol, runParser, safeMany)
import Text.Parsing.StringParser (Parser)
import Utils (aggregateExceptArray, readFile)

main :: Effect Unit
main = do
  result <- runExceptT mainExceptT
  case result of
    Left l -> Console.log ("error: " <> show l)
    Right r -> Console.log ("success: " <> show r)

mainExceptT :: ExceptT String Effect Int
mainExceptT = do
  inputString <- lift readInput >>= except # withExceptT show
  input <- runInputParser inputString
  except $ runExcept $ computeRiskLevelSum input

-- PARSE INPUT

readInput :: Effect (Either Error String)
readInput = readFile "data/9.txt"

runInputParser :: String -> ExceptT String Effect HeightMap
runInputParser inputString = runParser parseInput inputString # except # withExceptT show

parseInput :: Parser HeightMap
parseInput = do
  lines <- safeMany parseLine
  pure $ Array.fromFoldable lines

parseLine :: Parser HeightMapRow
parseLine = do
  line <- safeMany parseDigit
  _ <- parseEol
  pure $ HeightMapRow $ Array.fromFoldable line

-- PRIMITIVES

type Height = Int
newtype HeightMapRow = HeightMapRow (Array Height)
type HeightMap = Array HeightMapRow
type HeightMapCoord = { x :: Int, y :: Int}

getHeight :: HeightMap -> HeightMapCoord -> Maybe Height
getHeight heightMap { x, y } = do
  HeightMapRow row <- Array.index heightMap y
  Array.index row x

heightMapCoords :: HeightMap -> Array HeightMapCoord
heightMapCoords heightMap = Array.mapWithIndex (\y (HeightMapRow row) -> Array.mapWithIndex (\x _ -> { x, y }) row) heightMap # Array.concat

-- GLUE

computeRiskLevelSum :: HeightMap -> Except String Int
computeRiskLevelSum heightMap = do
  lowPoints <- findLowPoints heightMap 
  lowPoints <#> riskLevel heightMap # aggregateExceptArray <#> sum

-- LOGIC

riskLevel :: HeightMap -> HeightMapCoord -> Except String Int
riskLevel heightMap coord = except do
  height <- getHeight heightMap coord # note ("could not find risklevel for coord " <> show coord)
  pure $ height + 1

findLowPoints :: HeightMap -> Except String (Array HeightMapCoord)
findLowPoints heightMap = do
  exceptIsLowCoords <- go # aggregateExceptArray
  exceptIsLowCoords # Array.filter _.isLow <#> _.coord # pure
  where
  go :: Array (Except String {coord::HeightMapCoord, isLow :: Boolean})
  go = heightMapCoords heightMap 
    # map (\coord -> isLowPoint heightMap coord <#> \isLow -> { coord, isLow }) 

isLowPoint :: HeightMap -> HeightMapCoord -> Except String Boolean
isLowPoint heightMap coord = except do
  height <- getHeight heightMap coord # note ("could not find height for coord " <> show coord)
  pure $ Array.all (height < _) neighborHeights
  where
  neighborCoords =
    [ { x: coord.x, y: coord.y - 1}
    , { x: coord.x, y: coord.y + 1}
    , { x: coord.x - 1, y: coord.y}
    , { x: coord.x + 1, y: coord.y}
    ]
  neighborHeights = Array.mapMaybe (getHeight heightMap) neighborCoords
