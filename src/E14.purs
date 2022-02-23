module E14 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (Except, ExceptT, except, runExcept, runExceptT, withExceptT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseEol, runParser)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (anyLetter, string)
import Text.Parsing.StringParser.Combinators (many)
import Utils (readFile, unsafeFromJust)

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
  result <- except $ runExcept $ mainExcept input
  lift $ Console.log $ show $ result
  pure 0

-- PARSE INPUT

readInput :: Effect (Either Error String)
readInput = readFile "data/14-test.txt"

runInputParser :: String -> ExceptT String Effect Input
runInputParser inputString = runParser parseInput inputString # except # withExceptT show

parseInput :: Parser Input
parseInput = do
  template <- parseTemplate
  _ <- parseEol
  ruleMap <- many parseRule <#> Map.fromFoldable
  pure { template, ruleMap }

parseTemplate :: Parser PolymerTemplate
parseTemplate = do
  result <- many anyLetter <#> Array.fromFoldable
  _ <- parseEol
  pure result

parseRule :: Parser PairInsertionRule
parseRule = do
  char1 <- anyLetter
  char2 <- anyLetter
  _ <- string " -> "
  insertion <- anyLetter
  _ <- parseEol
  pure $ Tuple (Tuple char1 char2) insertion

-- PRIMITIVES

type PolymerTemplate = Array Char
type Pair = Tuple Char Char
type PairInsertionRule = Tuple Pair Char
type RuleMap = Map Pair Char

type Input = { template :: PolymerTemplate, ruleMap :: RuleMap }

-- GLUE

mainExcept :: Input -> Except String PolymerTemplate
mainExcept { template, ruleMap } = pure $ applyRulesOnce ruleMap template

applyRulesOnce :: RuleMap -> PolymerTemplate -> PolymerTemplate
applyRulesOnce ruleMap template = templatePairs template # applyRules ruleMap # concatInsertionResult

-- LOGIC

templatePairs :: PolymerTemplate -> Array Pair
templatePairs template = Array.zip (Array.dropEnd 1 template) (Array.drop 1 template)

applyRules :: RuleMap -> Array Pair -> Array (Array Char)
applyRules ruleMap pairs = pairs <#> applyRule
  where
  applyRule pair = case Map.lookup pair ruleMap of
    Nothing -> [ Tuple.fst pair, Tuple.snd pair ]
    Just insertion -> [ Tuple.fst pair, insertion, Tuple.snd pair ]

concatInsertionResult :: Array (Array Char) -> Array Char
concatInsertionResult chunks =
  let
    { head, tail } = unsafeFromJust "something went wrong with concat" $ Array.uncons chunks
    combine result chunk = result <> (Array.drop 1 chunk)
  in
    Array.foldl combine head tail
