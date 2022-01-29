module Iterator where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Rec.Class as TailRec
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unit as Unit

data IterationStep m a = Done | Yield a (IteratorT m a)
data IteratorT m a = IteratorT (Unit -> m (IterationStep m a))

getStep :: forall m a . IteratorT m a -> m (IterationStep m a)
getStep (IteratorT it) = it Unit.unit

empty :: forall m a . (Monad m) => IteratorT m a
empty = IteratorT (\_ -> pure Done)

singleton :: forall m a . (Monad m) => a -> IteratorT m a
singleton a = IteratorT (\_ -> pure $ Yield a empty)

liftIt :: forall m a . (Monad m) => m a -> IteratorT m a
liftIt ma = IteratorT (\_ -> ma >>= \a -> pure $ Yield a empty)

mapIt :: forall m a b . Monad m => (a -> b) -> IteratorT m a -> IteratorT m b
mapIt f it = IteratorT \_ -> getStep it >>= processStep
  where
  processStep :: IterationStep m a -> m (IterationStep m b)
  processStep step = pure case step of
    Done -> Done
    Yield value rest -> Yield (f value) (mapIt f rest)

applyIt :: forall m a b. Monad m => IteratorT m (a -> b) -> IteratorT m a -> IteratorT m b
applyIt mf ma = do
  f <- mf
  a <- ma
  pure $ f a

concat :: forall m a . Monad m => IteratorT m a -> IteratorT m a -> IteratorT m a
concat first second = IteratorT \_ -> getStep first >>= processStep
  where
  processStep :: IterationStep m a -> m (IterationStep m a)
  processStep step = case step of
    Done -> getStep second
    Yield value rest -> pure $ Yield value (concat rest second)

bindIt :: forall m a b . Monad m => IteratorT m a -> (a -> IteratorT m b) -> IteratorT m b
bindIt it f = IteratorT \_ -> getStep it >>= processStep
  where
  processStep :: IterationStep m a -> m (IterationStep m b)
  processStep step = case step of
    Done -> pure Done
    Yield value rest -> getStep $ concat (f value) (bindIt rest f)

unsafeFold :: forall m a b . (Monad m) => (b -> a -> b) -> b -> IteratorT m a -> m b
unsafeFold f b it = getStep it >>= processStep
  where
  processStep :: IterationStep m a -> m b
  processStep step = case step of
    Done -> pure b
    Yield value rest -> unsafeFold f (f b value) rest

-- tailRecM :: forall a b. (a -> m (Step a b)) -> a -> m b
fold :: forall m a b . (MonadRec m) => (b -> a -> b) -> b -> IteratorT m a -> m b
fold f b it = getStep it >>= \step -> TailRec.tailRecM processStep {step: step, acc: b}
  where
  processStep :: {step::IterationStep m a, acc::b} -> m (TailRec.Step {step::IterationStep m a, acc::b} b)
  processStep arg = case arg.step of
    Done -> pure (TailRec.Done arg.acc)
    Yield value rest -> getStep rest >>= \step -> pure (TailRec.Loop {step: step, acc: f arg.acc value})

unsafeFold' :: forall m a b . (Monad m) => (b -> a -> m b) -> b -> IteratorT m a -> m b
unsafeFold' f b it = getStep it >>= processStep
  where
  processStep :: IterationStep m a -> m b
  processStep step = case step of
    Done -> pure b
    Yield value rest -> f b value >>= \x -> unsafeFold' f x rest

filter :: forall m a . (Monad m) => (a -> Boolean) -> IteratorT m a -> IteratorT m a
filter pred iterator = IteratorT \_ -> getStep iterator >>= processStep
  where
  processStep :: IterationStep m a -> m (IterationStep m a)
  processStep step = case step of
    Done -> pure Done
    Yield value rest ->
      if pred value
      then pure $ Yield value $ filter pred rest
      else getStep rest >>= processStep

mapMaybe :: forall m a b. Monad m => (a -> Maybe b) -> IteratorT m a -> IteratorT m b
mapMaybe f iterator = IteratorT \_ -> doStep iterator
  where
  doStep :: IteratorT m a -> m (IterationStep m b)
  doStep it = getStep it >>= processStep
  processStep :: IterationStep m a -> m (IterationStep m b)
  processStep step = case step of
    Done -> pure Done
    Yield value rest -> case f value of
      Nothing -> doStep rest
      Just mappedValue -> pure $ Yield mappedValue $ mapMaybe f rest

toList :: forall m a . Monad m => IteratorT m a -> m (List a)
toList it =
  getStep it >>= \step -> case step of
    Done -> pure List.Nil
    Yield value rest -> toList rest >>= map pure (List.Cons value)

fromList :: forall a . List a -> IteratorT Identity a
fromList List.Nil = empty
fromList (List.Cons x xs) = IteratorT \_ -> pure $ Yield x (fromList xs)

-- TODO: make this tail recursive?
cartesianProduct :: forall a . List a -> List a -> IteratorT Identity (Tuple a a)
cartesianProduct List.Nil _ = empty
cartesianProduct _ List.Nil = empty
cartesianProduct (List.Cons x xs) ys = concat (combine x ys) (cartesianProduct xs ys)

allPairs :: forall a . List a -> IteratorT Identity (Tuple a a)
allPairs list = cartesianProduct list list

-- without duplicates
allPairs' :: forall a . List a -> IteratorT Identity (Tuple a a)
allPairs' list = allPairsRec List.Nil list
  where
  allPairsRec :: List a -> List a -> IteratorT Identity (Tuple a a)
  allPairsRec _ List.Nil = empty
  allPairsRec left (List.Cons x right) = concat (concat (combine x left) (combine x right)) (allPairsRec (List.Cons x left) right)

combine :: forall a . a -> List a -> IteratorT Identity (Tuple a a)
combine x others = fromList others <#> \y -> Tuple x y

instance (Monad m) => Functor (IteratorT m) where
  map = mapIt

instance (Monad m) => Apply (IteratorT m) where
  apply = applyIt

instance (Monad m) => Applicative (IteratorT m) where
  pure = singleton

instance (Monad m) => Bind (IteratorT m) where
  bind = bindIt

instance (Monad m) => Monad (IteratorT m)

instance MonadTrans IteratorT where
  lift = liftIt
