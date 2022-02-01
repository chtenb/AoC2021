module E9 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (ExceptT, except, runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (foldl, sum)
import Data.Int (fromString) as Int
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap, class Newtype)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import DebugUtils (debug_)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseDigit, parseEol, runParser, safeMany)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (regex, string)
import Text.Parsing.StringParser.Combinators (sepBy, sepEndBy)
import Utils (listOfEithersToEitherList, readFile)

main :: Effect Unit
main = do
  result <- runExceptT mainExceptT
  case result of
    Left l -> Console.log ("error: " <> show l)
    Right r -> Console.log ("success: " <> show r)

mainExceptT :: ExceptT String Effect Int
mainExceptT = do
  inputString <- lift readInput >>= except # withExceptT show
  inputs <- runInputParser inputString
  pure 0

-- PARSE INPUT

readInput :: Effect (Either Error String)
readInput = readFile "data/9-test.txt"

parseInput :: Parser HeightMap
parseInput = do
  lines <- safeMany parseLine
  pure $ Array.fromFoldable lines

parseLine :: Parser (Array Height)
parseLine = do
  line <- safeMany parseDigit
  _ <- parseEol
  pure $ Array.fromFoldable line

runInputParser inputString = runParser (safeMany parseInput) inputString # except # withExceptT show

-- PRIMITIVES

type Height = Int
type HeightMap = Array (Array Height)

-- GLUE

computeRiskLevelSum :: HeightMap -> Int
computeRiskLevelSum heightMap = 0

-- TRANSFORMATIONS
