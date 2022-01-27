module Iterator where

import Prelude

import Control.Monad.Trans.Class (class MonadTrans)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
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

fold :: forall m a b . (Monad m) => (b -> a -> b) -> b -> IteratorT m a -> m b
fold f b it = getStep it >>= processStep
  where
  processStep :: IterationStep m a -> m b
  processStep step = case step of
    Done -> pure b
    Yield value rest -> fold f (f b value) rest

fold' :: forall m a b . (Monad m) => (b -> a -> m b) -> b -> IteratorT m a -> m b
fold' f b it = getStep it >>= processStep
  where
  processStep :: IterationStep m a -> m b
  processStep step = case step of
    Done -> pure b
    Yield value rest -> f b value >>= \x -> fold' f x rest

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

instance (Monad m) => Functor (IteratorT m) where
  map = mapIt

instance (Monad m) => Apply (IteratorT m) where
  apply = applyIt

instance (Monad m) => Applicative (IteratorT m) where
  pure = singleton

instance (Monad m) => Bind (IteratorT m) where
  bind = bindIt

instance (Monad m) => MonadTrans IteratorT where
  lift = liftIt

fib :: Int -> Int -> IteratorT Identity Int
fib a b = IteratorT \_ -> if a < 100 then pure $ Yield (a+b) (fib b (a+b)) else pure Done

square :: forall m . (Monad m) => IteratorT m Int -> IteratorT m Int
square = mapIt \x -> x * x
