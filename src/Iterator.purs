module Iterator where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Data.Unit as Unit
import Effect (Effect)

data IterationStep a = End | Yield a (Iterator a)
data Iterator a = Iterator (Unit -> IterationStep a)

fib :: Int -> Int -> Iterator Int
fib a b = Iterator $ \_ -> if a < 100 then Yield (a+b) (fib b (a+b)) else End

square :: Iterator Int -> Iterator Int
square (Iterator numbers) = Iterator \_ -> case numbers Unit.unit of
  End -> End
  Yield number rest -> Yield (number * number) (square rest)

toList :: forall a . Iterator a -> List a
toList (Iterator iter) = iter Unit.unit # toList'
  where
  toList' :: IterationStep a -> List a
  toList' End = List.Nil
  toList' (Yield value rest) = value : toList rest

force :: forall a . Iterator a -> Unit
force (Iterator step) = case step Unit.unit of
  End -> Unit.unit
  Yield _ rest -> force rest
