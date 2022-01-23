module E2 where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Node.ReadLine (LineHandler)

import Iterator
import ReadLines (readLines)

main :: Effect Unit
main = do
  readLines "data/2-test.txt" # log
  -- stdout <- createWriteStream "/dev/stdout"
  -- _ <- writeString stdout UTF8 "test...\n" (pure Unit.unit)
  -- stdin <- createReadStream "/dev/stdin"
  -- interface <- createInterface stdin (Options [])
  -- setLineHandler handleLine interface


log :: IteratorT Effect String -> Effect Unit
log iter = fold f (pure unit) iter
  where
  f mb a = mb >>= \_ -> Console.log a
