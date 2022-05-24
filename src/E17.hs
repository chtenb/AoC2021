module Main where

import Test.QuickCheck
import Prelude

main :: IO ()
main = do
  putStrLn "Hello!"

xBounds = (241, 273)

yBounds = (-97, -63)

inBounds (lower, upper) pos = lower <= pos && pos <= upper

yHitsTarget pos speed = inBounds yBounds pos || if pos < fst yBounds then False else yHitsTarget (pos + speed) (speed -1)

xLandsTarget pos speed = if speed == 0 then inBounds xBounds pos else xLandsTarget (pos + speed) (speed -1)

xHitsTarget pos speed = inBounds xBounds pos || speed >= 0 && xHitsTarget (pos + speed) (speed -1)

maxAltitude pos speed = if speed <= 0 then pos else maxAltitude (pos + speed) (speed -1)

xCountSteps pos speed
  | speed < 0 = error "target miss"
  | inBounds xBounds pos = 0
  | otherwise = 1 + xCountSteps (pos + speed) (speed -1)

yCountSteps pos speed
  | pos < fst yBounds = error "target miss"
  | inBounds yBounds pos = 0
  | otherwise = 1 + yCountSteps (pos + speed) (speed -1)

hitsTarget (x, y) (dx, dy)
  | inBounds xBounds x && inBounds yBounds y = True
  | y < fst yBounds = False
  | otherwise = hitsTarget (x + dx, y + dy) (max 0 (dx -1), dy -1)

prop_1 x y dx dy = hitsTarget (x, y) (dx, dy) ==> (yHitsTarget y dy && xHitsTarget x dx)

-- (==>) :: Bool -> Bool -> Bool
-- (==>) a b = not (a && not b)

my_foldr :: (a -> b -> b) -> b -> [a] -> b
my_foldr _ b [] = b
my_foldr fabb b (a : as') = fabb a (my_foldr fabb b as')
