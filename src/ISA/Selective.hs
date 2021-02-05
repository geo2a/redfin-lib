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
    (Prop(..), elimProp
    , Selective(..), selectM, ifS, whenS
    , S.Over(..)) where

import qualified Control.Selective as S

instance Monoid m => Selective (S.Over m) where
  select (S.Over x) _ (S.Over y) = S.Over (x <> y)

-- | An equality check is either true or false,
--   or we can't tell
data Prop a = Trivial Bool
            | Nontrivial a
            deriving (Show, Functor)

elimProp :: Monoid a => Prop a -> a
elimProp = \case Trivial _ -> mempty
                 Nontrivial x -> x

-- | This flavour of Selective functors dispatches on
--   an effectful equality check and applies one of the
--   provided effectful eliminators accordingly:
--   * if the check is 'Trivial' it eliminates the boolean
--     into 'f b', and the effect associated with the second
--     eliminator 'f (a -> b)' may be skipped;
--   * otherwise it applies must apply the effectful eliminator
--     in order to deliver 'f b'
--   We still need to prove that this formulation is equivalent
--   to the one from the paper, I guess.
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

-- | Conditionally perform an effect
--   We need 'a' to be a monoid here, but if 'whenS' would
--   only needed to return 'f ()', like in the paper, we
--   could have dropped this constraint
whenS :: (Selective f,  Monoid a) => f (Prop a) -> f a -> f a
whenS condition x =
  select condition (\cond -> if cond then x else pure mempty) (pure id)
