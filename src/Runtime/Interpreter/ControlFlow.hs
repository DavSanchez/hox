{-# LANGUAGE PatternSynonyms #-}

module Runtime.Interpreter.ControlFlow (ControlFlow (..), pattern Return) where

import Data.Bifunctor (Bifunctor (bimap))

{-# COMPLETE Return, Continue #-}

data ControlFlow b c
  = Break b
  | Continue c
  deriving stock (Show, Eq)

-- | Pattern synonym for 'Break' to represent a return value in control flow.
pattern Return :: b -> ControlFlow b c
pattern Return x = Break x

instance Functor (ControlFlow b) where
  fmap :: (a -> c) -> ControlFlow b a -> ControlFlow b c
  fmap _ (Break v) = Break v
  fmap f (Continue a) = Continue (f a)

instance Applicative (ControlFlow b) where
  pure :: a -> ControlFlow b a
  pure = Continue

  (<*>) :: ControlFlow b (a -> c) -> ControlFlow b a -> ControlFlow b c
  Break v <*> _ = Break v
  Continue f <*> r = fmap f r

instance Monad (ControlFlow b) where
  (>>=) :: ControlFlow b a -> (a -> ControlFlow b c) -> ControlFlow b c
  Break v >>= _ = Break v
  Continue x >>= f = f x

instance Bifunctor ControlFlow where
  bimap :: (b -> d) -> (a -> c) -> ControlFlow b a -> ControlFlow d c
  bimap f _ (Break v) = Break (f v)
  bimap _ g (Continue a) = Continue (g a)
