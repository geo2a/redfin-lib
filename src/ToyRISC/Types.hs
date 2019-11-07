{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Types
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Types describing ToyRISC ISA
--
-----------------------------------------------------------------------------

module ToyRISC.Types
    ( -- * Types describing architecture concepts
      -- ** data register
      Register (..)
      -- ** memory location
    , Address (..)
      -- ** flag
    , Flag (..)
      -- immediate agument
    , Imm (..)
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

import           Data.Int      (Int32)
import           Data.Typeable
import           Data.Word     (Word16)
import           Debug.Trace

-- | Data registers
data Register = R0 | R1
  deriving (Show, Eq, Ord)

-- | Memory location
newtype Address = Address Word16
  deriving (Show, Eq, Ord, Num)

-- | Flag
data Flag = Condition
  deriving (Show, Eq, Ord)

-- | Immediate argument
newtype Imm a = Imm a
  deriving (Show, Eq, Num)

newtype Data a = MkData a
  deriving (Show, Eq, Ord, Num, Typeable)

-----------------------------------------------------------------------------

data Key where
  Reg :: Register -> Key
  Addr :: Address -> Key
  F :: Flag -> Key
  IC :: Key

deriving instance Eq Key
deriving instance Ord Key

instance Show Key where
    show = \case
        Reg  reg   -> show reg
        Addr addr  -> show addr
        F    flag  -> show flag
        IC         -> "IC"
        -- IR         -> "IR"
        -- Prog addr  -> show addr

-- data Key a where
--     Reg  :: Register -> Key Int32
--     -- ^ register
--     Addr :: Address -> Key Int32
--     -- ^ memory address
--     F   :: Flag -> Key Bool
--     -- -- ^ flag
--     IC   :: Key Int32
--     -- -- ^ instruction counter
--     -- IR   :: Key (InstructionCode)
--     -- -- ^ instruction register
--     -- Prog :: InstructionAddress -> Key (InstructionCode)
--     -- -- ^ program memory address

-- deriving instance Eq a => Eq (Key a)
-- deriving instance Ord a => Ord (Key a)

-- instance Show (Key a) where
--     show = \case
--         Reg  reg   -> show reg
--         Addr addr  -> show addr
--         F    flag  -> show flag
--         IC         -> "IC"
--         -- IR         -> "IR"
--         -- Prog addr  -> show addr

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

data Equality a = Solvable Bool
                | Unsolvable a
                deriving (Show, Typeable)


instance Show a => Boolean (Equality a) where
  true = Solvable True
  not  = error "Equality.Boolean.not: not is undefined"
  toBool t = case t of
    Solvable b   -> b
    Unsolvable x -> -- True
                    trace (show t) True
  (|||) = error "Equality.Boolean.|||: undefined"
  (&&&) = error "Equality.Boolean.&&&: undefined"

-- | This class abstracts an equality check with possible failure, i.e. in the
--   case when the values are symbolic. In case of concrete types with an 'Eq'
--   instance '(===)' will always return @Right Bool@.
class TryEq a where
  (===) :: a -> a -> Equality a

-- instance Eq a => TryEq (Data a) where
--   (MkData x) === (MkData y) = Solvable (x == y)

instance TryEq (Data Int32) where
  (MkData x) === (MkData y) = Solvable (x == y)

instance Semigroup (Data Int32) where
  (<>) = (+)

instance Monoid (Data Int32) where
  mempty = 0

-- | We now consider a value to be a numeric monoid which could also be converted
--   into booleans
type Value a = (Typeable a, Show a, TryEq a, Num a, Boolean a, Monoid a)
-----------------------------------------------------------------------------
