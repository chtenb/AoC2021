module E5 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import Iterator (allPairs', fold)
import ParserUtils (parseEol, parseInt, runParser, safeMany)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (string)
import Utils (readFile)

readInput :: Effect (Either Error String)
readInput = readFile "data/5.txt"

handleLeft :: forall a b . Show a => Either a b -> MaybeT Effect b
handleLeft = case _ of
  Left l -> MaybeT $ Console.log (show l) >>= \_ -> pure Nothing
  Right r -> MaybeT $ pure $ Just r

main :: Effect Unit
main = do
  _ <- runMaybeT main'
  pure unit

log :: List LineSegment -> Effect Unit
log ls = do
  _ <- sequence $ ls >>= \l -> pure $ Console.log (show l <> " contains " <> show (pointsOnLine l))
  pure unit

main' :: MaybeT Effect Unit
main' = do
  inputString <- lift readInput >>= handleLeft
  input <- runParser parseInput inputString # handleLeft
  -- lift $ log input
  output <- pure (compute input)
  -- lift $ Console.log $ show output
  lift $ Console.log $ show (Set.size $ output)
  pure unit

compute :: List LineSegment -> Set Point
compute input = fold f Set.empty (allPairs' input) # unwrap
  where
  f :: Set Point -> Tuple LineSegment LineSegment -> Set Point
  f intersections (Tuple l1 l2) = Set.union (intersect l1 l2) intersections

getLineType :: LineSegment -> LineType
getLineType l =
  if getX1 l == getX2 l
  then Vertical
  else if getY1 l == getY2 l
  then Horizontal
  else Other

pointsOnLine :: LineSegment -> Set Point
pointsOnLine l = case getLineType l of
  Vertical -> Set.fromFoldable $ Int.range (getY1 l) (getY2 l) <#> \y -> Point (getX1 l) y
  Horizontal -> Set.fromFoldable $ Int.range (getX1 l) (getX2 l) <#> \x -> Point x (getY1 l)
  Other -> Set.fromFoldable $ List.zip (Int.range (getX1 l) (getX2 l)) (Int.range (getY1 l) (getY2 l)) <#> \(Tuple x y) -> Point x y

intersect :: LineSegment -> LineSegment -> Set Point
intersect l1 l2 = result -- debug ("intersection between " <> show l1 <> " and " <> show l2 <> " is " <> show result) result
  where 
  result = Set.intersection (pointsOnLine l1) (pointsOnLine l2)


-- PARSER

parseInput :: Parser (List LineSegment)
parseInput = safeMany parseLine

parsePoint :: Parser Point
parsePoint = do
  x <- parseInt
  _ <- string ","
  y <- parseInt
  pure $ Point x y

parseLine :: Parser LineSegment
parseLine = do
  p1 <- parsePoint
  _ <- string " -> "
  p2 <- parsePoint
  _ <- parseEol
  pure $ LineSegment p1 p2

-- DATA

data Point = Point Int Int
getX :: Point -> Int
getX (Point x _) = x
getY :: Point -> Int
getY (Point _ y) = y

instance Eq Point where
  -- eq :: a -> a -> Boolean
  eq p1 p2 = getX p1 == getX p2 && getY p1 == getY p2

instance Ord Point where
  -- compare :: a -> a -> Ordering
  compare p1 p2 = case compare (getX p1) (getX p2) of
    EQ -> compare (getY p1) (getY p2)
    a -> a

data LineType = Vertical | Horizontal | Other
derive instance Generic LineType _
instance Show LineType where
  show = genericShow

data LineSegment = LineSegment Point Point
getX1 :: LineSegment -> Int
getX1 (LineSegment p1 _) = getX p1
getX2 :: LineSegment -> Int
getX2 (LineSegment _ p2) = getX p2
getY1 :: LineSegment -> Int
getY1 (LineSegment p1 _) = getY p1
getY2 :: LineSegment -> Int
getY2 (LineSegment _ p2) = getY p2

instance Show LineSegment where
  show (LineSegment p1 p2) = show p1 <> "  ->  " <> show p2

instance Show Point where
  show (Point x y) = "(" <> show x <> "," <> show y <> ")"
