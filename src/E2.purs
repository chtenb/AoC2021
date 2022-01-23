module E2 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Options (Options(..))
import Data.Unit as Unit
import Effect (Effect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.ReadLine (LineHandler, createInterface, setLineHandler)
import Node.Stream (onDataString, readEither, readString, writeString)

import Iterator
import ReadLines (readLines)

main :: Effect Unit
main = do
  pure $ force $ readLines "data/2-test.txt"
  -- stdout <- createWriteStream "/dev/stdout"
  -- _ <- writeString stdout UTF8 "test...\n" (pure Unit.unit)
  -- stdin <- createReadStream "/dev/stdin"
  -- interface <- createInterface stdin (Options [])
  -- setLineHandler handleLine interface

handleLine :: LineHandler Unit
handleLine input = Console.log $ "handling line: " <> input

processInput :: String -> Effect Unit
processInput input = Console.log input
