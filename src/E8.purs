module E8 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Foldable (foldl, minimum, sum)
import Data.List (List)
import Data.List as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseEol, parseInt, runParser)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (string)
import Text.Parsing.StringParser.Combinators (sepBy)
import Utils (readFile)

readInput :: Effect (Either Error String)
readInput = readFile "data/7.txt"

handleLeft :: forall a b . Show a => Either a b -> MaybeT Effect b
handleLeft = case _ of
  Left l -> MaybeT $ Console.log ("error: " <> show l) >>= \_ -> pure Nothing
  Right r -> MaybeT $ pure $ Just r

parseInput :: Parser (List Crab)
parseInput = sepBy parseInt (string ",") <* parseEol

main :: Effect Unit
main = runMaybeT main' *> pure unit

main' :: MaybeT Effect Unit
main' = do
  inputString <- lift readInput >>= handleLeft
  input <- runParser parseInput inputString # handleLeft
  maxLocation <- pure (foldl max 0 input)
  bestLocationCost <- pure $ Int.range 0 maxLocation <#> (alignmentCost input) # minimum
  lift $ Console.log $ "max loc " <> show (foldl max 0 input)
  lift $ Console.log $ "nr crabs " <> show (List.length input)
  lift $ Console.log $ "best loc " <> show bestLocationCost
  pure unit

type Crab = Int
type Location = Int
type Fuel = Int

alignmentCostForCrab :: Location -> Crab -> Fuel
alignmentCostForCrab loc crab = n * (n+1) / 2
  where
  n = abs (loc - crab)

alignmentCost :: List Crab -> Location -> Fuel
alignmentCost crabs loc = crabs <#> alignmentCostForCrab loc # sum

