module ParserUtils where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Text.Parsing.StringParser (Parser(..), fail, try, unParser)
import Text.Parsing.StringParser.CodeUnits (anyDigit, regex)
import Text.Parsing.StringParser.Combinators (lookAhead, many, many1, many1Till, manyTill)

parseEol :: Parser String
parseEol = regex """\n\r?|\r"""

-- NOTES
-- many p will get stuck in a loop if p possibly doesn't consume any input but still succeeds
-- many (many p) will get stuck for any p
-- parseEndOfLine doesn't consume input at the end of the file but still succeeds

-- TODO: make pull request for this combinator
-- Fails with parse error if parser did not consume any input
assertConsume :: forall a. Parser a -> Parser a
assertConsume p = Parser $ \posStrBefore ->
  case unParser p posStrBefore of
    Right result ->
      if posStrBefore.pos < result.suffix.pos
      then Right result
      else Left { pos: result.suffix.pos, error: "Consumed no input." }
    x -> x

safeMany :: forall a. Parser a -> Parser (List a)
safeMany = many <<< assertConsume

safeMany1 :: forall a. Parser a -> Parser (NonEmptyList a)
safeMany1 = many1 <<< assertConsume

safeManyTill :: forall a end. Parser a -> Parser end -> Parser (List a)
safeManyTill p = manyTill (assertConsume p)

safeMany1Till :: forall a end. Parser a -> Parser end -> Parser (NonEmptyList a)
safeMany1Till p = many1Till (assertConsume p)

safeLookAhead :: forall a. Parser a -> Parser a
safeLookAhead = try <<< lookAhead

parseSpaces1 :: Parser String
parseSpaces1 = regex """[ ]+"""

parseSpaces :: Parser String
parseSpaces = regex """[ ]*"""

parseInt :: Parser Int
parseInt =
  many anyDigit <#> Array.fromFoldable <#> String.fromCharArray <#> Int.fromString
    >>= \maybeInt -> case maybeInt of
      Nothing -> (fail "error parsing int")
      Just int -> pure int
