-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Selective
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- A special kind of Selective Applicative functors
--
-----------------------------------------------------------------------------

module ISA.Selective
    (Selective(..)) where

-- -- | Branch on a Boolean value, skipping unnecessary effects.
-- ifS :: Selective f => f Bool -> f a -> f a -> f a
-- ifS x t e = branch (bool (Right ()) (Left ()) <$> x) (const <$> t) (const <$> e)

class Applicative f => Selective f where
  branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c

  select :: f (Either a b) -> f (a -> b) -> f b
  select x y = branch x y (pure id)

data Equality a = Trivial Bool
                | NonTrivial a

elimEquality :: b -> b -> (a -> b) -> Equality a -> b
elimEquality onTrue onFalse onNonTrivial = \case
  Trivial True  -> onTrue
  Trivial False -> onFalse
  NonTrivial x -> onNonTrivial x

-- decideEq :: Selective f => f (Equality a) -> f b -> f b -> f b -> f b
-- decideEq x onTrue onFalse onNonTrivial =
--   branch (elimEquality onFasle ontrue onNonTrivial <$> x)
