----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Prop
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Boolean formulas that may be symbolic
--
-----------------------------------------------------------------------------
module ISA.Types.Prop
    (-- ** Booleans
      Boolean (..)
    -- ** equality and order checks that may fail
    , TryEq (..), TryOrd(..)
    , Prop(..), elimProp
    ) where

import           Prelude hiding (not)
import qualified Prelude

-- | An equality check is either true or false,
--   or we can't tell
data Prop a where
  Trivial :: Boolean a => Bool -> Prop a
  Nontrivial :: a -> Prop a

-- | Eliminate a @Prop@ if it's trivial, or leave it as is
elimProp :: Boolean a => Prop a -> a
elimProp = \case Trivial b    -> fromBool b
                 Nontrivial x -> x

deriving instance Show a => Show (Prop a)

class Boolean a where
  toBool  :: a -> Bool
  fromBool :: Bool -> a
  true    :: a
  false   :: a
  false = not true
  not     :: a -> a

  (|||)   :: a -> a -> a
  (&&&)   :: a -> a -> a

instance Boolean Bool where
  true = True
  not = Prelude.not
  toBool = id
  fromBool = id

  x ||| y = x || y
  x &&& y = x && y

instance Boolean a => Boolean (Prop a) where
  true = Trivial True
  not  = error "Prop.Boolean.not: not is undefined"
  toBool t = case t of
    Trivial b    -> b
    Nontrivial _ -> True
  fromBool b = Trivial b
  x ||| y =
    case (x, y) of
      (Trivial a, Trivial b)       -> Trivial (a || b)
      (Trivial a , Nontrivial b)   -> if a then Trivial True else Nontrivial b
      (Nontrivial a, Trivial b)    -> if b then Trivial True else Nontrivial a
      (Nontrivial a, Nontrivial b) -> Nontrivial (a ||| b)
  x &&& y =
    case (x, y) of
      (Trivial a, Trivial b)       -> Trivial (a && b)
      (Trivial a , Nontrivial b)   -> if a then Nontrivial b else Trivial False
      (Nontrivial a, Trivial b)    -> if b then Nontrivial a else Trivial False
      (Nontrivial a, Nontrivial b) -> Nontrivial (a &&& b)
    -- error "Prop.Boolean.|||: undefined"
  -- (&&&) = error "Prop.Boolean.&&&: undefined"

-- | This class abstracts an equality check with possible failure, i.e. in the
--   case when the values are symbolic. In case of concrete types with an 'Eq'
--   instance '(===)' will always return @Trivial@.
class TryEq a where
  (===) :: a -> a -> Prop a

-- | Similar for TryEq, but for strict order
class TryEq a => TryOrd a where
  lt :: a -> a -> Prop a
  gt :: a -> a -> Prop a
