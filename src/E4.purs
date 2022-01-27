module E4 where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.List (List, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList, fromList, toList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import DebugUtils (debug, debug_)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import ParserUtils (parseEol, parseInt, parseSpaces, parseSpaces1, runParser, safeMany1)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (string)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Utils (readFile)

readInput :: Effect (Either Error String)
readInput = readFile "data/4.txt"

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
  scores <- pure $ findLosingScores input
  lift $ Console.log $ show scores
  pure unit

type DrawnNumbers = NonEmptyList Int
data BoardCell = BoardCell Int Boolean
instance Show BoardCell where
  show (BoardCell i b) = "(" <> show i <> "," <> show b <> ")"
type BoardRow = NonEmptyList BoardCell
type Board = NonEmptyList BoardRow
type Score = Int
data GameState = GameState DrawnNumbers (NonEmptyList Board)

type PartialBoardRow = List BoardCell
type PartialBoard = List PartialBoardRow

toPartialBoard :: Board -> PartialBoard
toPartialBoard board = toList $ map toList board
fromPartialBoard :: Partial => PartialBoard -> Board
fromPartialBoard partialBoard = unsafeFromList $ map unsafeFromList partialBoard
  where
  unsafeFromList :: forall a . List a -> NonEmptyList a
  unsafeFromList list = fromList list # fromJust

printBoardRow :: PartialBoardRow -> String
printBoardRow List.Nil = ""
printBoardRow (c:cs) = show c <> "   " <> printBoardRow cs
printBoard :: PartialBoard -> String
printBoard List.Nil = ""
printBoard (row:rows) = printBoardRow row <> "\n" <> printBoard rows

transposeBoard :: Board -> Board
transposeBoard board = unsafePartial $ fromPartialBoard $ transposePartialBoard $ toPartialBoard board

transposePartialBoard :: PartialBoard -> PartialBoard
transposePartialBoard List.Nil = List.Nil
transposePartialBoard original =
  case extractColumn original of
    Nothing -> List.Nil
    Just (Tuple columnAsRow remBoard) -> columnAsRow : transposePartialBoard remBoard

extractColumn :: PartialBoard -> Maybe (Tuple PartialBoardRow PartialBoard)
extractColumn (List.Nil) = Nothing
extractColumn (List.Nil:_) = Nothing
extractColumn ((h:t):List.Nil) = Just $ Tuple (h:List.Nil) (t:List.Nil)
extractColumn ((h:t):rest) = 
  case extractColumn rest of
    Just (Tuple hs ts) -> Just $ Tuple (h : hs) (t : ts)
    Nothing -> Nothing

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
hasBingo board = checkAllCells board || checkAllCells (transposeBoard board)
  where
  checkAllCells = List.any (List.all cellIsChecked)

getUncheckedNumbers :: Board -> List Int
getUncheckedNumbers board = List.concatMap (NEL.mapMaybe f) (toList board)
  where
  f (BoardCell number checked) = 
    if checked
    then Nothing
    else Just number

getBoardScore :: Int -> Board -> Int
getBoardScore lastNumber board = lastNumber * (List.foldl (+) 0 $ getUncheckedNumbers board)

findLosingScores :: GameState -> List Score
findLosingScores (GameState draws boards) = findLosingScores' (toList draws) (toList boards)
  where
  findLosingScores' :: List Int -> List Board -> List Score
  findLosingScores' List.Nil _ = List.Nil
  findLosingScores' (draw:remainingNumbers) currentBoards = let
    newBoards = currentBoards <#> checkNumber (debug_ draw)
    nonWinningBoards = List.filter (not hasBingo) newBoards
    in case nonWinningBoards of
      List.Nil -> newBoards <#> \b -> getBoardScore draw (debugBoard b)
      bs -> findLosingScores' remainingNumbers bs

findWinningScores :: GameState -> List Score
findWinningScores (GameState draws boards) = findWinningScores' (toList draws) boards
  where
  findWinningScores' :: List Int -> NonEmptyList Board -> List Score
  findWinningScores' List.Nil _ = List.Nil
  findWinningScores' (draw:remainingNumbers) currentBoards = let
    newBoards = currentBoards <#> checkNumber (debug_ draw)
    winningBoards = NEL.filter hasBingo newBoards
    in case winningBoards of
      List.Nil -> findWinningScores' remainingNumbers newBoards
      w -> w <#> \b -> getBoardScore draw (debugBoard b)

debugBoard :: NonEmptyList (NonEmptyList BoardCell) -> NonEmptyList (NonEmptyList BoardCell)
debugBoard b = debug (printBoard (toPartialBoard b) <> "\n\n" <> printBoard (toPartialBoard (transposeBoard b))) b

parseInput :: Parser GameState
parseInput = do
  drawn <- parseDrawnNumbers
  boards <- safeMany1 parseBoard
  pure $ GameState drawn boards

parseDrawnNumbers :: Parser DrawnNumbers
parseDrawnNumbers = sepBy1 parseInt (string ",") <* parseEol

parseBoard :: Parser Board
parseBoard = parseEol *> safeMany1 parseBoardRow

parseBoardRow :: Parser BoardRow
parseBoardRow = sepBy1 (parseSpaces *> parseInt) parseSpaces1 <* parseEol <#> \ints -> map (\i -> BoardCell i false) ints
