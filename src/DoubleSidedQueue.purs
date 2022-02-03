module DoubleSidedQueue where

import Prelude

import Control.Alt ((<|>))
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))

-- x : xs Front , Back xs : x
newtype DoubleSidedQueue a = DoubleSidedQueue { backStack :: List a, frontStack :: List a }

pushBack :: forall a. a -> DoubleSidedQueue a -> DoubleSidedQueue a
pushBack elem (DoubleSidedQueue queue) = DoubleSidedQueue (queue { backStack = elem : queue.backStack })

empty :: forall a. DoubleSidedQueue a
empty = DoubleSidedQueue { backStack: List.Nil, frontStack: List.Nil }

takeHalf :: forall a. List a -> { firstHalf :: List a, secondHalf :: List a }
takeHalf list =
  let
    pivot = Int.ceil $ Int.toNumber (List.length list) / 2.0
    front = List.take pivot list
    back = List.drop pivot list
  in
    { firstHalf: front, secondHalf: back }

rebalanceToFront :: forall a. DoubleSidedQueue a -> DoubleSidedQueue a
rebalanceToFront (DoubleSidedQueue queue) =
  let
    { firstHalf, secondHalf } = takeHalf queue.backStack
  in
    DoubleSidedQueue { backStack: firstHalf, frontStack: queue.frontStack <> List.reverse secondHalf }

popFront :: forall a. DoubleSidedQueue a -> Maybe ({ head :: a, tail :: DoubleSidedQueue a })
popFront queue = tryPopFront queue <|> (rebalanceToFront queue # tryPopFront)
  where
  tryPopFront (DoubleSidedQueue { backStack, frontStack }) =
    case frontStack of
      List.Nil -> Nothing
      (x : xs) -> Just $ { head: x, tail: DoubleSidedQueue { frontStack: xs, backStack: backStack } }

drop1 :: forall a. DoubleSidedQueue a -> DoubleSidedQueue a
drop1 q = case popFront q of
  Nothing -> q
  Just { tail } -> tail

reverse :: forall a. DoubleSidedQueue a -> DoubleSidedQueue a
reverse (DoubleSidedQueue { backStack, frontStack }) = DoubleSidedQueue { backStack: List.reverse frontStack, frontStack: List.reverse backStack }

fromList :: forall a. List a -> DoubleSidedQueue a
fromList list = DoubleSidedQueue { backStack: List.Nil, frontStack: list }

toList :: forall a. DoubleSidedQueue a -> List a
toList (DoubleSidedQueue { backStack, frontStack }) = frontStack <> List.reverse backStack

merge :: forall a. DoubleSidedQueue a -> DoubleSidedQueue a -> DoubleSidedQueue a
merge q1 q2 = fromList (toList q1 <> toList q2)

-- make it a double sided queue with same performance characteristics
