module E2 where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Console as Console
import Iterator (IteratorT, empty, fold, singleton, singleton')
import ReadLines (readLines)
import Text.Parsing.StringParser (Parser, ParseError, fail, unParser)
import Text.Parsing.StringParser.CodeUnits (anyDigit, eof, string)
import Text.Parsing.StringParser.Combinators (many)

main :: Effect Unit
main = do
  readInputLines
  # log
  <#> runParser parseInstruction
  # handleParseErrors 
  # fold processInstruction2 initialSubmarineState
  # computeEndResult2
  >>= \answer -> Console.log $ show answer

computeEndResult2 :: Effect SubmarineState -> Effect Int
computeEndResult2 eff = do
  { horizontal, depth } <- eff
  pure $ horizontal * depth

computeEndResult1 :: Effect PositionPart1 -> Effect Int
computeEndResult1 eff = do
  { horizontal, vertical } <- eff
  pure $ horizontal * vertical

-- TODO: this looks like a specific instance of the bind function
handleParseErrors :: forall a . IteratorT Effect (Either ParseError a) -> IteratorT Effect a
handleParseErrors iterator = iterator >>= f
  where
  f :: Either ParseError a -> IteratorT Effect a
  f eitherValue = 
    case eitherValue of
      Left err -> singleton' (Console.error (show err)) >>= \_ -> empty
      Right value -> singleton value

log :: forall a . Show a => IteratorT Effect a -> IteratorT Effect a
log it = it >>= f
  where
  f :: a -> IteratorT Effect a
  f a = singleton' $ Console.log (show a) >>= \_ -> pure a

data Instruction = Forward Int | Down Int | Up Int
type PositionPart1 = { horizontal :: Int, vertical :: Int }
type SubmarineState = { horizontal :: Int, depth :: Int, aim :: Int }

initialSubmarineState :: { aim :: Int , depth :: Int , horizontal :: Int }
initialSubmarineState = { horizontal : 0, depth : 0, aim : 0 }

initialPosition :: PositionPart1
initialPosition = { horizontal : 0, vertical : 0 }

readInputLines :: IteratorT Effect String
readInputLines = readLines "data/2.txt"

parseInstruction :: Parser Instruction
parseInstruction = parseForward <|> parseDown <|> parseUp

parseForward :: Parser Instruction
parseForward = string "forward " *> parseInt <#> Forward
parseUp :: Parser Instruction
parseUp = string "up " *> parseInt <#> Up
parseDown :: Parser Instruction
parseDown = string "down " *> parseInt <#> Down

parseInt :: Parser Int
parseInt =
  many anyDigit <#> Array.fromFoldable <#> String.fromCharArray <#> Int.fromString
    >>= \maybeInt -> case maybeInt of
      Nothing -> (fail "error")
      Just int -> pure int

processInstruction2 :: SubmarineState -> Instruction -> SubmarineState
processInstruction2 state instruction = case instruction of
  Down i -> state { aim = state.aim + i }
  Up i -> state { aim = state.aim - i }
  Forward i -> state { horizontal = state.horizontal + i, depth = state.depth + state.aim * i }

processInstruction1 :: PositionPart1 -> Instruction -> PositionPart1
processInstruction1 position instruction = case instruction of
  Forward i -> position { horizontal = position.horizontal + i }
  Down i -> position { vertical = position.vertical + i }
  Up i -> position { vertical = position.vertical - i }

runParser :: forall a. Parser a -> String -> Either ParseError a
runParser parser inputString = unParser (parser <* eof) { str: inputString, pos: 0 } <#> _.result
