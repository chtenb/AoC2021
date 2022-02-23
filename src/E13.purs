module E13 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (Except, ExceptT, except, runExcept, runExceptT, withExceptT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Int (rem)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits as String
import Debug (spy, spyWith)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseEol, parseInt, runParser)
import Partial (crash)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (string)
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
  -- lift $ Console.log $ show $ countPoints input.paper
  -- _ <- pure $ spy "hi" input
  result <- except $ runExcept $ mainExcept input
  _ <- pure $ spyWith "result " (Array.fromFoldable) result
  lift $ printPaper result
  pure 0

-- PARSE INPUT

readInput :: Effect (Either Error String)
readInput = readFile "data/13.txt"

runInputParser :: String -> ExceptT String Effect Input
runInputParser inputString = runParser parseInput inputString # except # withExceptT show

parseInput :: Parser Input
parseInput = do
  points <- many parsePoint
  _ <- parseEol
  instructions <- many parseInstruction
  pure { paper: Set.fromFoldable points, instructions: Array.fromFoldable instructions }

parsePoint :: Parser Point
parsePoint = do
  x <- parseInt
  _ <- string ","
  y <- parseInt
  _ <- parseEol
  pure $ { x, y }

parseInstruction :: Parser FoldInstruction
parseInstruction =
  let
    parseAlongX = string "x=" *> parseInt <#> AlongX
    parseAlongY = string "y=" *> parseInt <#> AlongY
  in
    do
      _ <- string "fold along "
      result <- parseAlongX <|> parseAlongY
      _ <- parseEol
      pure result

-- GLUE

mainExcept :: Input -> Except String Paper
mainExcept { paper, instructions } = applyFoldInstructions instructions paper # pure

--- PRIMITIVES

type Point = { y :: Int, x :: Int }
-- type Point = { x :: Int, y :: Int }
type Paper = Set Point
data FoldInstruction = AlongY Int | AlongX Int
type Input = { paper :: Paper, instructions :: Array FoldInstruction }

-- LOGIC

countPoints :: Paper -> Int
countPoints = Set.size

applyFoldInstructions :: Array FoldInstruction -> Paper -> Paper
applyFoldInstructions instructions paper = Array.foldl applyFoldInstruction paper instructions

applyFoldInstruction :: Paper -> FoldInstruction -> Paper
applyFoldInstruction paper instruction = case instruction of
  AlongX foldAxis -> Set.map (foldPointAlongX foldAxis) paper
  AlongY foldAxis -> Set.map (foldPointAlongY foldAxis) paper

foldPointAlongY :: Int -> Point -> Point
foldPointAlongY foldAxis point = point { y = newY }
  where
  newY = if foldAxis < point.y then -(point.y - foldAxis) + foldAxis else point.y

foldPointAlongX :: Int -> Point -> Point
foldPointAlongX foldAxis point = point { x = newX }
  where
  newX = if foldAxis < point.x then -(point.x - foldAxis) + foldAxis else point.x

printPaper :: Paper -> Effect Unit
printPaper paper =
  let
    xMax = Set.map _.x paper # maximum # unsafeFromJust "no maximum x found"
    yMax = Set.map _.y paper # maximum # unsafeFromJust "no maximum y found"

    showPoint :: Int -> Int -> Char
    showPoint y x = if Set.member { x, y } paper then '#' else '.'

    showLine :: Int -> Array Char
    showLine y = Array.range 0 xMax <#> showPoint y

    printArrayChar :: Array Char -> Effect Unit
    printArrayChar chars = String.fromCharArray chars # Console.log
  in
    Array.range 0 yMax <#> showLine # Array.foldMap printArrayChar

