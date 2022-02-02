module Queue where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))

newtype Queue a = Queue { pushStack :: List a, popStack :: List a }

push :: forall a. a -> Queue a -> Queue a
push elem (Queue queue) = Queue (queue { pushStack = elem : queue.pushStack })

empty :: forall a. Queue a
empty = Queue { pushStack: List.Nil, popStack: List.Nil }

pop :: forall a. Queue a -> Maybe ({ head :: a, tail :: Queue a })
pop (Queue queue) = case queue.popStack of
  List.Nil ->
    case List.reverse queue.pushStack of
      List.Nil -> Nothing
      (x : xs) -> Just $ { head: x, tail: Queue { pushStack: List.Nil, popStack: xs } }
  (x : xs) -> Just $ { head: x, tail: Queue queue { popStack = xs } }

drop1 :: forall a. Queue a -> Queue a
drop1 q = case pop q of
  Nothing -> q
  Just { tail } -> tail

reverse :: forall a. Queue a -> Queue a
reverse (Queue { pushStack, popStack }) = Queue { pushStack: List.reverse popStack, popStack: List.reverse pushStack }

fromList :: forall a. List a -> Queue a
fromList list = Queue { pushStack: List.Nil, popStack: list }

toList :: forall a. Queue a -> List a
toList (Queue { pushStack, popStack }) = popStack <> List.reverse pushStack

merge :: forall a. Queue a -> Queue a -> Queue a
merge q1 q2 = fromList (toList q1 <> toList q2)

-- make it a double sided queue with same performance characteristics
