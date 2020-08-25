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
    (Prop(..)
    , Selective(..), selectM, ifS, whenS
    , S.Over(..)) where

import qualified Control.Selective as S

instance Monoid m => Selective (S.Over m) where
  select (S.Over x) _ (S.Over y) = S.Over (x <> y)

data Prop a = Trivial Bool
            | Nontrivial a

class Applicative f => Selective f where
  select :: f (Prop a) -> (Bool -> f b) -> f (a -> b) -> f b
  -- whenProp :: f (Prop a) -> f a -> f

selectM :: Monad f => f (Prop a) -> f (Bool -> b) -> f (a -> b) -> f b
selectM condition boolElim dataElim = do
  c <- condition
  case c of
    Trivial b    -> ($ b) <$> boolElim
    Nontrivial d -> ($ d) <$> dataElim

-- | Branch on a Boolean value, skipping unnecessary effects.
ifS :: Selective f => f Bool -> f a -> f a -> f a
ifS x t e = select (Trivial <$> x) (\x -> if x then t else e) (pure id)

whenS :: (Selective f,  Monoid a) => f (Prop a) -> f a -> f a
whenS condition x =
  select condition (\cond -> if cond then x else pure mempty) (pure id)
