module Iterator where

import Prelude

import Control.Monad.List.Trans (ListT(..))
import Control.Monad.List.Trans as ListT
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Unit as Unit

data IterationStep m a = Done | Yield a (IteratorT m a)
data IteratorT m a = IteratorT (Unit -> m (IterationStep m a))

empty :: forall m a . (Monad m) => IteratorT m a
empty = IteratorT (\_ -> pure Done)

map :: forall m a b . Monad m => (a -> b) -> IteratorT m a -> IteratorT m b
map f (IteratorT it) = IteratorT \_ -> it Unit.unit >>= processStep
  where
  processStep :: IterationStep m a -> m (IterationStep m b)
  processStep step = pure case step of
    Done -> Done
    Yield value rest -> Yield (f value) (map f rest)

fold :: forall m a b . (Monad m) => (m b -> a -> m b) -> m b -> IteratorT m a -> m b
fold f b (IteratorT it) = it Unit.unit >>= processStep
  where
  processStep :: IterationStep m a -> m b
  processStep step = case step of
    Done -> b
    Yield value rest -> fold f (f b value) rest

filter :: forall m a . (Monad m) => (a -> Boolean) -> IteratorT m a -> IteratorT m a
filter pred iterator = IteratorT \_ -> doStep iterator
  where
  doStep :: IteratorT m a -> m (IterationStep m a)
  doStep (IteratorT it) = it Unit.unit >>= processStep
  processStep :: IterationStep m a -> m (IterationStep m a)
  processStep step = case step of
    Done -> pure Done
    Yield value rest ->
      if pred value
      then pure $ Yield value $ filter pred rest
      else doStep rest

mapMaybe :: forall m a b. Monad m => (a -> Maybe b) -> IteratorT m a -> IteratorT m b
mapMaybe f iterator = IteratorT \_ -> doStep iterator
  where
  doStep :: IteratorT m a -> m (IterationStep m b)
  doStep (IteratorT it) = it Unit.unit >>= processStep
  processStep :: IterationStep m a -> m (IterationStep m b)
  processStep step = case step of
    Done -> pure Done
    Yield value rest -> case f value of
      Nothing -> doStep rest
      Just mappedValue -> pure $ Yield mappedValue $ mapMaybe f rest

toList :: forall m a . (Monad m) => IteratorT m a -> ListT m a
toList (IteratorT it) =
  ListT $ it Unit.unit >>= \step -> case step of
    Done -> pure ListT.Done
    Yield value rest -> pure $ ListT.Yield value $ defer \_ -> toList rest

-- untilLeft :: forall m l r . IteratorT m (Either l r) -> Either (IteratorT m l) (m r)
-- untilLeft (IteratorT it) = it Unit.unit >>= processStep
--   where
--   processStep :: IterationStep m a -> Either (IteratorT m l) (m r)
--   processStep step = case step of
--     Done -> Right Done
--     Yield value rest -> fold f (f b value) rest

fib :: Int -> Int -> IteratorT Identity Int
fib a b = IteratorT \_ -> if a < 100 then pure $ Yield (a+b) (fib b (a+b)) else pure Done

square :: forall m . (Monad m) => IteratorT m Int -> IteratorT m Int
square = map \x -> x * x


instance (Monad m) => Functor (IteratorT m) where
  map = map

-- instance (Monad m) => Unfoldable1 (IteratorT m) where
--   unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> IteratorT m a
--   unfoldr1 f b = go (f b)
--     where
--     -- go :: forall a b. Tuple a (Maybe b) -> IteratorT m a
--     go thing = IteratorT \_ -> pure $ 
--       case thing of
--         Tuple x Nothing -> Yield x (IteratorT \_ -> Done)
--         Tuple x (Just y) -> Yield x (go (f y))

-- instance (Monad m) => Unfoldable (IteratorT m) where
--   unfoldr f b = go (f b)
--     where
--       go = case _ of
--         Nothing -> Done
--         Just (Tuple x y) -> Yield (pure x) (defer \_ -> (go (f y)))
