module E2 where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.List (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Console as Console
import Iterator (IteratorT, empty, fold, mapMaybe)
import ReadLines (readLines)
import Text.Parsing.StringParser (Parser, ParseError, fail, unParser)
import Text.Parsing.StringParser.CodeUnits (anyDigit, eof, string)
import Text.Parsing.StringParser.Combinators (many)

main :: Effect Unit
main = do
  readInputLines
  <#> runParser parseInstruction
  # handleParseErrors 
  # fold processInstruction (pure initialPosition)
  # computeEndResult
  >>= \answer -> Console.log $ show answer

computeEndResult :: Effect Position -> Effect Int
computeEndResult eff = do
  (Position { horizontal, vertical }) <- eff
  pure $ horizontal * vertical

-- TODO
handleParseErrors :: forall a . IteratorT Effect (Either ParseError a) -> IteratorT Effect a
handleParseErrors _ = empty
  where
  handle :: Either ParseError String -> Effect (Maybe String)
  handle = case _ of
    Left err -> do
      Console.error $ show err
      pure Nothing
    Right result -> pure $ Just result

log :: IteratorT Effect String -> Effect Unit
log iter = fold f (pure unit) iter
  where
  f effect a = effect >>= \_ -> Console.log a

data Instruction = Forward Int | Down Int | Up Int
newtype Position = Position { horizontal :: Int, vertical :: Int }
unwrap :: Position -> { horizontal :: Int, vertical :: Int }
unwrap (Position p) = p

initialPosition :: Position
initialPosition = Position { horizontal : 0, vertical : 0 }

readInputLines :: IteratorT Effect String
readInputLines = readLines "data/2-test.txt"

parseInstruction :: Parser Instruction
parseInstruction = parseForward <|> parseDown <|> parseUp

parseForward :: Parser Instruction
parseForward = string "foward " *> parseInt <#> Forward
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

-- TODO: can we make this pure?
processInstruction :: Effect Position -> Instruction -> Effect Position
processInstruction eff instruction = do
  (Position position) <- eff
  pure $ Position case instruction of
    Forward i -> position { horizontal = position.horizontal + i }
    Down i -> position { vertical = position.vertical + i }
    Up i -> position { vertical = position.vertical - i }

runParser :: forall a. Parser a -> String -> Either ParseError a
runParser parser inputString = unParser (parser <* eof) { str: inputString, pos: 0 } <#> _.result
