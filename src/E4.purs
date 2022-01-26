module E4 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.List (List, (:))
import Data.List as List
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (drop)
import Data.Tuple (Tuple(..))
import DebugUtils (debug_)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseEol, parseInt, parseSpaces, parseSpaces1, safeMany1)
import Text.Parsing.StringParser (Parser, unParser)
import Text.Parsing.StringParser.CodeUnits (eof, string)
import Text.Parsing.StringParser.Combinators (sepBy, sepBy1)
import Utils (readFile)

readInput :: Effect (Either Error String)
readInput = readFile "data/4-test.txt"

type DrawnNumbers = List Int
data BoardCell = BoardCell Int Boolean
instance Show BoardCell where
  show (BoardCell i b) = show i <> "," <> show b
type BoardRow = List BoardCell
type Board = List BoardRow
type Score = Int
data GameState = GameState DrawnNumbers (List Board)

extractColumn :: Board -> Tuple BoardRow Board
extractColumn List.Nil = Tuple List.Nil List.Nil
extractColumn (List.Nil:rows) = extractColumn  rows
extractColumn ((x:xs):rows) = Tuple (x : rec) (xs : board)
  where
  Tuple rec board = extractColumn rows

transposeBoard :: Board -> Board
transposeBoard List.Nil = List.Nil
transposeBoard original = columnAsRow : transposeBoard remBoard
  where
  Tuple columnAsRow remBoard = extractColumn original

checkCell :: Int -> BoardCell -> BoardCell
checkCell number (BoardCell cellNumber checked) = 
  if number == cellNumber
  then BoardCell cellNumber true
  else BoardCell cellNumber checked

checkNumber :: Int -> Board -> Board
checkNumber number board = map (map $ checkCell number) board

cellIsChecked :: BoardCell -> Boolean
cellIsChecked (BoardCell _ checked) = checked

hasBingo :: Board -> Boolean
hasBingo board = checkAllCells board && checkAllCells (transposeBoard board)
  where
  checkAllCells = List.any (List.all cellIsChecked)

getUncheckedNumbers :: Board -> List Int
getUncheckedNumbers = List.concatMap (List.mapMaybe f)
  where
  f (BoardCell number checked) = 
    if checked
    then Nothing
    else Just number

getBoardScore :: Int -> Board -> Int
getBoardScore lastNumber board = lastNumber * (List.foldl (+) 0 $ getUncheckedNumbers board)

handleLeft :: forall a b . Show a => Either a b -> MaybeT Effect b
handleLeft = case _ of
  Left l -> MaybeT $ Console.log (show l) >>= \_ -> pure Nothing
  Right r -> MaybeT $ pure $ Just r

main :: Effect Unit
main = do
  _ <- runMaybeT main'
  pure unit

main' :: MaybeT Effect Unit
main' = do
  inputString <- lift readInput >>= handleLeft
  input <- runParser parseInput inputString # handleLeft
  scores <- pure $ playGame input
  lift $ Console.log $ show scores
  pure unit

playGame :: GameState -> List Score
playGame (GameState (List.Nil) _) = List.Nil
playGame (GameState (draw:remainingNumbers) boards) = let
  newBoards = boards <#> checkNumber draw
  winningBoards = List.filter hasBingo newBoards
  in case debug_ winningBoards of
    List.Nil -> playGame $ GameState remainingNumbers newBoards
    w -> w <#> getBoardScore draw


runParser :: forall a. Parser a -> String -> Either {position::String,error::String,suffix::String} a
runParser parser inputString = case unParser (parser <* eof) { str: inputString, pos: 0 } of
  Left rec -> Left msg
    where
    msg =
      { position: show rec.pos
      , error: rec.error
      , suffix: (drop rec.pos inputString) }
  Right rec -> do
    pure rec.result

parseInput :: Parser GameState
parseInput = do
  drawn <- parseDrawnNumbers
  boards <- safeMany1 parseBoard
  pure $ GameState drawn (toList boards)

parseDrawnNumbers :: Parser DrawnNumbers
parseDrawnNumbers = sepBy parseInt (string ",") <* parseEol

parseBoard :: Parser Board
parseBoard = parseEol *> safeMany1 parseBoardRow <#> toList

parseBoardRow :: Parser BoardRow
parseBoardRow = sepBy1 (parseSpaces *> parseInt) parseSpaces1 <* parseEol <#> \ints -> toList $ map (\i -> BoardCell i false) ints
