module E12 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (Except, ExceptT, except, runExcept, runExceptT, withExceptT)
import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Debug (spy, spyWith)
import DebugUtils (debug)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseDigit, parseEol, runParser, safeMany)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (regex, string)
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
  -- _ <- pure $ spy "hi" input
  except $ runExcept $ mainExcept input

-- PARSE INPUT

readInput :: Effect (Either Error String)
readInput = readFile "data/12.txt"

runInputParser :: String -> ExceptT String Effect CaveGraph
runInputParser inputString = runParser parseInput inputString # except # withExceptT show

parseInput :: Parser CaveGraph
parseInput = do
  edges <- safeMany parseEdge
  pure $ List.foldl processEdge Map.empty edges
  where
  processEdge :: CaveGraph -> Tuple Cave Cave -> CaveGraph
  processEdge graph (Tuple node1 node2) = graph # Map.alter (addNode node1) node2 # Map.alter (addNode node2) node1

  addNode :: Cave -> Maybe CaveNeighborhood -> Maybe CaveNeighborhood
  addNode cave maybeNeighbors = Just case maybeNeighbors of
    Nothing -> [ cave ]
    Just neighbors -> cave : neighbors

parseEdge :: Parser (Tuple Cave Cave)
parseEdge = do
  node1 <- parseAnyCave
  _ <- string "-"
  node2 <- parseAnyCave
  _ <- parseEol
  pure $ Tuple node1 node2

parseAnyCave :: Parser Cave
parseAnyCave = parseBigCave <|> parseSmallCave

parseBigCave :: Parser Cave
parseBigCave = regex "[A-Z]+" <#> \name -> Big name

parseSmallCave :: Parser Cave
parseSmallCave = regex "[a-z]+" <#> \name -> Small name

-- PRIMITIVES

data Cave = Big String | Small String

derive instance Eq Cave
derive instance Ord Cave

type CaveNeighborhood = Array Cave
type CaveGraph = Map Cave CaveNeighborhood

start :: Cave
start = Small "start"

end :: Cave
end = Small "end"

-- GLUE

mainExcept :: CaveGraph -> Except String Int
mainExcept graph = pure (countAllPaths graph)

-- LOGIC

countAllPaths :: CaveGraph -> Int
countAllPaths graph = go Set.empty start
  where
  go :: Set Cave -> Cave -> Int
  go visited currentCave =
    case currentCave of
      Small "end" -> flip const (spy "found path " (Array.fromFoldable visited)) 1
      Small _ ->
        if Set.member currentCave visited then 0
        else (getNeighbors currentCave) <#> (\neighbor -> go (Set.insert currentCave visited) neighbor) # sum
      Big _ -> (getNeighbors currentCave) <#> (\neighbor -> go visited neighbor) # sum

  getNeighbors :: Cave -> Array Cave
  getNeighbors cave = case Map.lookup cave graph of
    Nothing -> unsafePartial $ crashWith "could not find cave"
    Just neighbors -> neighbors
