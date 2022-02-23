{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Array (Array, array, bounds, (!))
import Data.Bifunctor (Bifunctor (first))
import Data.Char (Char, ord)
import Data.Foldable (Foldable (foldMap), all)
import qualified Data.List as List
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PSQueue
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (Parsec, digit, many, parse)
import Prelude

main :: IO ()
main = do
  putStrLn "Hello!"
  input <- readInput
  case run input of
    Left msg -> putStrLn msg
    Right risk -> do
      putStrLn "Success!!"
      putStrLn $ "Risk is:" ++ show risk

run :: [String] -> Except Risk
run input = do
  risks <- parseRisks input
  grid <- risksToGrid risks
  dijkstra grid (targetPos grid) initialQueue Set.empty

type Except = Either String

-- Invariant: the value is positive
-- Read as (x,y)
type Pos = (Int, Int)

-- Invariant: the value is greater than 0
type Risk = Int

-- Invariant: the array is square
type Grid = Array Pos Risk

-- CONSTRUCT

grid :: Int -> [(Pos, Risk)] -> Except Grid
grid size items =
  let itemsAreValid = all (\(pos, _) -> posIsValid size pos) items
   in if itemsAreValid then Right $ array ((0, 0), (size, size)) items else Left "Invalid bounds"

cell :: Int -> Int -> Risk -> (Pos, Risk)
cell y x risk = ((x, y), risk)

readInput :: IO [String]
readInput = lines <$> readFile "data/15.txt"

parseRisks :: [String] -> Except [[Risk]]
parseRisks rows = first show $ traverse (parse parseRiskLine "") rows
  where
    parseRiskLine :: Parsec String () [Risk]
    parseRiskLine = many (parseRisk <$> digit)

    parseRisk :: Char -> Int
    parseRisk digit = ord digit - 48

risksToGrid :: [[Risk]] -> Except Grid
risksToGrid risks =
  let transformRow :: Int -> [Risk] -> [(Pos, Risk)]
      transformRow rowNr = mapWithIndex (cell rowNr)
   in grid (List.length risks) $ foldMapWithIndex transformRow risks

foldMapWithIndex :: Monoid b => (Int -> a -> b) -> [a] -> b
foldMapWithIndex f items = foldMap (uncurry f) (zip [0 ..] items)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

-- SEARCH

getRisk :: Grid -> Pos -> Risk
getRisk grid pos =
  let size = baseGridSize grid
      (x, y) = pos
      baseX = x `mod` size
      baseY = y `mod` size
      nrTilesX = x `div` size
      nrTilesY = y `div` size
      basePos = (baseX, baseY)
      baseRisk = grid ! basePos
      risk = ((baseRisk + nrTilesX + nrTilesY -1) `mod` 9) + 1
   in risk

baseGridSize :: Grid -> Int
baseGridSize grid = result
  where
    (_, (_, result)) = bounds grid

gridSize :: Grid -> Int
gridSize grid = baseGridSize grid * 5

neighbors :: Grid -> Pos -> [Pos]
neighbors grid (x, y) = List.filter (posIsValid (gridSize grid)) [(x -1, y), (x, y -1), (x + 1, y), (x, y + 1)]

posIsValid :: Int -> Pos -> Bool
posIsValid size (x, y) = 0 <= x && x < size && 0 <= y && y < size

targetPos :: Grid -> Pos
targetPos grid = dupe $ gridSize grid - 1

type Queue = PSQ Pos Risk

initialQueue :: Queue
initialQueue = PSQueue.singleton (0, 0) 0

dupe :: a -> (a, a)
dupe x = (x, x)

dijkstra :: Grid -> Pos -> Queue -> Set Pos -> Except Risk
dijkstra grid target queue visited =
  case PSQueue.findMin queue of
    Nothing -> Left "queue empty"
    Just binding ->
      if pos == target
        then Right risk
        else dijkstra grid target newQueue (Set.insert pos visited)
      where
        pos = PSQueue.key binding
        risk = PSQueue.prio binding
        newQueue = PSQueue.delete pos $ Prelude.foldl updateNeighborRisks queue (List.filter (`Set.notMember` visited) (neighbors grid pos))
        updateNeighborRisks queue' neighborPos = PSQueue.alter (maybeMin (risk + neighborRisk)) neighborPos queue'
          where
            neighborRisk = getRisk grid neighborPos

maybeMin :: Int -> Maybe Int -> Maybe Int
maybeMin x Nothing = Just x
maybeMin x (Just y) = Just $ min x y
