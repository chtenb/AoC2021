module E9 where

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

mainExcept :: HeightMap -> Except String Int
mainExcept input = computeThreeLargestBasinSizesSum input

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
type Basin = Set HeightMapCoord

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

computeThreeLargestBasinSizesSum :: HeightMap -> Except String Int
computeThreeLargestBasinSizesSum heightMap = do
  sizes <- pure $ computeBasinSizes heightMap
  sorted <- pure $ List.reverse $ List.sort sizes
  case sorted of
    x : y : z : _ -> pure $ x * y * z
    _ -> throwError $ "could not sum " <> show sorted

computeBasinSizes :: HeightMap -> List Int
computeBasinSizes heightMap = findAllBasins heightMap <#> Set.size

-- LOGIC

findAllBasins :: HeightMap -> List Basin
findAllBasins heightMap = go Set.empty (basinCoords heightMap # List.fromFoldable) List.Nil
  where
  go :: Set HeightMapCoord -> List HeightMapCoord -> List Basin -> List Basin
  go _ List.Nil resultAcc = resultAcc
  go allExploredCoords (List.Cons x xs) resultAcc = 
    if Set.member x allExploredCoords
    then go allExploredCoords xs resultAcc
    else go (Set.union allExploredCoords newBasin) xs (List.Cons newBasin resultAcc)
    where newBasin = exploreBasin heightMap x

exploreBasin :: HeightMap -> HeightMapCoord -> Basin
exploreBasin heightMap startCoord = go Set.empty (List.singleton startCoord)
  where
  go :: Set HeightMapCoord -> List HeightMapCoord -> Set HeightMapCoord
  go explored List.Nil = explored
  go explored (List.Cons x xs) = 
    if Set.member x explored
    then go explored xs
    else go (Set.insert x explored) (append (neighborhood x) xs)
  neighborhood x = basinNeighborCoords heightMap x # List.fromFoldable

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
  neighborHeights = Array.mapMaybe (getHeight heightMap) (basinNeighborCoords heightMap coord)

-- Array of neighbors that are in a basin (i.e. they exist and are not of height 9)
basinNeighborCoords :: HeightMap -> HeightMapCoord -> Array HeightMapCoord
basinNeighborCoords heightMap coord =
  [ { x: coord.x, y: coord.y - 1}
  , { x: coord.x, y: coord.y + 1}
  , { x: coord.x - 1, y: coord.y}
  , { x: coord.x + 1, y: coord.y}
  ] # Array.filter (isInBasin heightMap)

-- Array of coords that are in a basin (i.e. they exist and are not of height 9)
basinCoords :: HeightMap -> List HeightMapCoord
basinCoords heightMap = heightMapCoords heightMap # Array.filter (isInBasin heightMap) # List.fromFoldable

isInBasin :: HeightMap -> HeightMapCoord -> Boolean
isInBasin heightMap coord = case getHeight heightMap coord of
  Nothing -> false
  Just height -> height < 9
