module Test.Iterator where

import Iterator
import Prelude
import Test.QuickCheck
import Test.Utils

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console as Console
import Test.QuickCheck.Arbitrary (class Arbitrary)


main :: Effect Unit
main = do
  assertSuccess testFoldItRecStackSafety
  -- assertSuccess testFoldItStackSafety
  -- assertSuccess testCoRecursionStackSafety
  -- _ <- pure $ maybeInc 1
  pure unit

testFoldItRecStackSafety :: Result
testFoldItRecStackSafety = assertEquals (fold min 0 (incUntil 0 10000000) # unwrap) 0
  where
  incUntil :: Int -> Int -> IteratorT Identity Int
  incUntil i max = IteratorT \_ -> if i < max then pure $ Yield i (incUntil (i+1) max) else pure Done

{-
maybeInc :: Int -> Maybe Int
maybeInc 0 = Just 0
maybeInc x = Just (x+1) >>= maybeInc

testCoRecursionStackSafety :: Result
testCoRecursionStackSafety = assertEquals 0 (foo 1000000)
  where
  foo 0 = 0
  foo x = bar (x+1)
  bar x = foo (x-2)

testFoldItStackSafety :: Result
testFoldItStackSafety = assertEquals (fold (+) 0 (incUntil 0 1000000) # unwrap) 1000000
  where
  incUntil :: Int -> Int -> IteratorT Identity Int
  incUntil i max = IteratorT \_ -> if i < max then pure $ Yield i (incUntil (i+1) max) else pure Done

-- GENERATORS

fib :: Int -> Int -> IteratorT Identity Int
fib a b = IteratorT \_ -> if a < 100 then pure $ Yield (a+b) (fib b (a+b)) else pure Done

square :: forall m . (Monad m) => IteratorT m Int -> IteratorT m Int
square = mapIt \x -> x * x
-}
