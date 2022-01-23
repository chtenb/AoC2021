module Iterator where

import Prelude

import Control.Monad.List.Trans (ListT(..))
import Control.Monad.List.Trans as ListT
import Data.Identity (Identity)
import Data.Lazy (defer)
import Data.Unit as Unit

data IterationStep m a = Done | Yield a (IteratorT m a)
data IteratorT m a = IteratorT (Unit -> m (IterationStep m a))

instance (Monad m) => Functor (IteratorT m) where
  map :: forall a b. (a -> b) -> IteratorT m a -> IteratorT m b
  map f (IteratorT iterator) =
    IteratorT \_ -> iterator Unit.unit 
      >>= \step -> case step of
        Done -> pure Done
        Yield value rest -> pure $ Yield (f value) (map f rest)

fold :: forall m a b . (Monad m) => (m b -> a -> m b) -> m b -> IteratorT m a -> m b
fold f b (IteratorT iterator) = iterator Unit.unit >>= foldStep
    where
    foldStep :: IterationStep m a -> m b
    foldStep = \step -> case step of
      Done -> b
      Yield value rest -> fold f (f b value) rest

toList :: forall m a . (Monad m) => IteratorT m a -> ListT m a
toList (IteratorT iterator) =
  ListT $ iterator Unit.unit >>= \step -> case step of
    Done -> pure ListT.Done
    Yield value rest -> pure $ ListT.Yield value $ defer \_ -> toList rest

fib :: Int -> Int -> IteratorT Identity Int
fib a b = IteratorT \_ -> if a < 100 then pure $ Yield (a+b) (fib b (a+b)) else pure Done

square :: forall m . (Monad m) => IteratorT m Int -> IteratorT m Int
square = map \x -> x * x


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
