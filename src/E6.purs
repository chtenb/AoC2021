module E6 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import DebugUtils (debug_)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseEol, parseInt, runParser)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (string)
import Text.Parsing.StringParser.Combinators (sepBy)
import Utils (readFile)

readInput :: Effect (Either Error String)
readInput = readFile "data/6.txt"

handleLeft :: forall a b . Show a => Either a b -> MaybeT Effect b
handleLeft = case _ of
  Left l -> MaybeT $ Console.log (show l) >>= \_ -> pure Nothing
  Right r -> MaybeT $ pure $ Just r

-- TODO: use big ints

-- log :: List LineSegment -> Effect Unit
-- log ls = do
--   _ <- sequence $ ls >>= \l -> pure $ Console.log (show l <> " contains " <> show (pointsOnLine l))
--   pure unit

main :: Effect Unit
main = runMaybeT main' *> pure unit

main' :: MaybeT Effect Unit
main' = do
  inputString <- lift readInput >>= handleLeft
  input <- runParser parseInput inputString # handleLeft
  population <- pure $ simulateDays 256 input
  count <- pure $ countTotalPopulation population
  -- lift $ log input
  -- output <- pure (compute input)
  lift $ Console.log $ show count
  -- lift $ Console.log $ show (Set.size $ output)
  pure unit

type FishTimer = Int
type FishCount = BigInt

type FishPopulation = Map FishTimer FishCount
toList :: FishPopulation -> List (Tuple FishTimer FishCount)
toList = Map.toUnfoldable
fromList :: List (Tuple FishTimer FishCount) -> FishPopulation
fromList = Map.fromFoldable

safeLookup :: FishTimer -> FishPopulation -> FishCount
safeLookup timer population = Map.lookup timer population # Maybe.fromMaybe (fromInt 0)

safeInsert :: FishTimer -> FishCount -> FishPopulation -> FishPopulation
safeInsert timer count population = Map.insert timer (safeLookup timer population + count) population

safeReinsert :: FishTimer -> (FishCount -> Tuple FishTimer FishCount) -> FishPopulation -> FishPopulation
safeReinsert timer f population = f (safeLookup timer population) # \(Tuple newTimer count) -> population # Map.delete timer # safeInsert newTimer count

countTotalPopulation :: FishPopulation -> BigInt
countTotalPopulation = foldl (+) (fromInt 0)

simulateDays :: Int -> FishPopulation -> FishPopulation
simulateDays 0 population = population
simulateDays days population = simulateDays (days-1) (simulateDay population)

simulateDay :: FishPopulation -> FishPopulation
simulateDay = ageUp >>> reproduce >>> recharge

ageUp :: FishPopulation -> FishPopulation
ageUp population = population # toList # map (\(Tuple timer count) -> Tuple (timer - 1) count) # fromList

reproduce :: FishPopulation -> FishPopulation
reproduce population = case Map.lookup (-1) population of
  Just newFishCount -> Map.insert 8 newFishCount population
  Nothing -> population

recharge :: FishPopulation -> FishPopulation
recharge population = safeReinsert (-1) (\count -> Tuple 6 count) population

parseInput :: Parser FishPopulation
parseInput = sepBy parseInt (string ",") <* parseEol <#> bucketize
  where
  bucketize = foldl (\population timer -> safeInsert timer (fromInt 1) population) Map.empty
