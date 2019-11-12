{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Types describing ISA ISA
--
-----------------------------------------------------------------------------

module ISA.Types
    ( -- * Types describing architecture concepts
      -- ** data register
      Register (..)
      -- ** memory location
    , Address (..)
      -- ** flag
    , Flag (..)
      -- immediate argument
    , Imm (..)
    -- instruction code
    , InstructionCode (..)
    -- packaged data
    , Data (..)
    -- equality check that may fail
    , Equality(..), TryEq (..)
    -- * Abstraction over possible locations in the ISA
    , Key(..)

    -- * Classes abstracting values that ISA model can operate with
    -- ** Booleans
    , Boolean (..)
    , Value
    ) where

import           Data.Bits
import           Data.Int        (Int32)
import           Data.Typeable
import           Data.Word       (Word16)
import           Generic.Random
import           GHC.Generics    (Generic)
import           Test.QuickCheck (Arbitrary, arbitrary)

-- | Data registers
data Register = R0 | R1 | R2 | R3
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary Register where
  arbitrary = genericArbitrary uniform

-- | Memory location
newtype Address = Address Word16
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Bounded, Bits, FiniteBits, Generic)

instance Arbitrary Address where
  arbitrary = genericArbitrary uniform

-- | Flag
data Flag = Halted
          | Condition
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary Flag where
  arbitrary = genericArbitrary uniform

-- | Immediate argument
newtype Imm a = Imm a
  deriving (Functor, Show, Eq, Num, Bits, FiniteBits, Generic)

instance Arbitrary a => Arbitrary (Imm a) where
  arbitrary = genericArbitrary uniform

newtype InstructionCode a = InstructionCode a
  deriving (Functor, Show, Eq, Ord, Num, Bits, FiniteBits, Generic)

instance Arbitrary a => Arbitrary (InstructionCode a) where
  arbitrary = genericArbitrary uniform

newtype Data a = MkData a
  deriving (Functor, Show, Eq, Ord, Num, Typeable, Bounded, Bits, FiniteBits, Generic)

instance Arbitrary a => Arbitrary (Data a) where
  arbitrary = genericArbitrary uniform

-----------------------------------------------------------------------------

data Key where
  Reg :: Register -> Key
  -- ^ data register
  Addr :: Address -> Key
  -- ^ memory cell
  F :: Flag -> Key
  -- ^ flag, a special boolean register
  IC :: Key
  -- ^ instruction counter
  IR :: Key
  -- ^ instruction register
  Prog :: Address -> Key
  -- ^ program address

deriving instance Eq Key
deriving instance Ord Key

instance Show Key where
    show = \case
        Reg  reg   -> show reg
        Addr addr  -> show addr
        F    flag  -> show flag
        IC         -> "IC"
        IR         -> "IR"
        Prog addr  -> show addr

class Boolean a where
  toBool  :: a -> Bool
  true    :: a
  not     :: a -> a

  (|||)   :: a -> a -> a
  (&&&)   :: a -> a -> a

instance Boolean Bool where
  true = True
  not = Prelude.not
  toBool = id

  x ||| y = x || y
  x &&& y = x && y

instance Boolean (Data Int32) where
  toBool (MkData x) = x /= 0
  true = MkData 1
  not  (MkData x) = if x == 0 then 1 else 0

  x ||| y = if (toBool x ||| toBool y) then 1 else 0
  x &&& y = if (toBool x &&& toBool y) then 1 else 0


data Equality a = Trivial Bool
                | Nontrivial a
                deriving (Show, Typeable)

instance Boolean (Equality a) where
  true = Trivial True
  not  = error "Equality.Boolean.not: not is undefined"
  toBool t = case t of
    Trivial b    -> b
    Nontrivial _ -> True
  (|||) = error "Equality.Boolean.|||: undefined"
  (&&&) = error "Equality.Boolean.&&&: undefined"

-- | This class abstracts an equality check with possible failure, i.e. in the
--   case when the values are symbolic. In case of concrete types with an 'Eq'
--   instance '(===)' will always return @Right Bool@.
class TryEq a where
  (===) :: a -> a -> Equality a

-- instance Eq a => TryEq (Data a) where
--   (MkData x) === (MkData y) = Trivial (x == y)

instance TryEq (Data Int32) where
  (MkData x) === (MkData y) = Trivial (x == y)

instance Semigroup (Data Int32) where
  (<>) = (+)

instance Monoid (Data Int32) where
  mempty = 0

-- | We now consider a value to be a numeric monoid which could also be converted
--   into booleans
type Value a = (Show a, TryEq a, Monoid a, Num a, Boolean a)
-----------------------------------------------------------------------------
