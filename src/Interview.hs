{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Bifunctor (Bifunctor (first))
import Data.Kind (Type)
import Prelude hiding (length, (++))

main = do
  pure ()

data List a = Nil | Cons a (List a)

length :: List a -> Int
length Nil = 0
length (Cons x xs) = 1 + length xs

(++) :: List a -> List a -> List a
(++) Nil ys = ys
(++) (Cons x xs) ys = Cons x (xs ++ ys)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = fmap f xs ++ (fs <*> xs)

instance Monad List where
  (>>=) Nil f = Nil
  (>>=) (Cons x xs) f = f x ++ (xs >>= f)

newtype Parser a = Parser {getParser :: String -> [(a, String)]}

instance Functor Parser where
  fmap f p = Parser {getParser = mapParseResult . getParser p}
    where
      mapParseResult = map (first f)

instance Applicative Parser where
  pure :: a -> Parser a
  pure value = Parser {getParser = \s -> [(value, s)]}

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- (<*>) pf pa = Parser {getParser = applyPa . getParser pf}
  --   where
  --     -- applyPa :: [(a -> b, String)] -> [(b, String)]
  --     applyPa parseResult1 = parseResult1 >>= applyPa'
  --     -- applyPa' :: (a -> b, String) -> [(b, String)]
  --     applyPa' (f, s') = map (first f) parseResult2
  --       where
  --         -- parseResult2 :: [(a, String)]
  --         parseResult2 = getParser pa s'
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf pa = do
    f <- pf
    f <$> pa

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa pf = Parser {getParser = result}
    where
      result s = getParser pa s >>= \(a, s') -> getParser (pf a) s'

-- FoldableMonoid ::= (* -> *) -> * -> *
newtype FoldableMonoid (m :: Type -> Type) a = FoldableMonoid (m a)

instance (Foldable m, Monoid (m a), Monoid (m b)) => Monad (FoldableMonoid m)
