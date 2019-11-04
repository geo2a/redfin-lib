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
    , Data (..)
    -- * Abstraction over possible locations in the ISA
    , Key(..)

    -- * Classes abstracting values that ISA model can operate with
    -- ** Booleans
    , Boolean (..)
    , Value
    ) where

import           Data.Word (Word16)

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
  deriving (Show, Eq, Ord, Num)

-----------------------------------------------------------------------------

-- | This type enumerates data locations in the ISA model
data Key = Reg Register
         | Addr Address
         | F Flag
         | IC -- ^ instruction counter (aka program counter)
         deriving (Eq, Ord)

instance Show Key where
  show = \case
    Reg r -> show r
    Addr a -> show a
    F flag -> show flag
    IC -> "IC"

class Boolean a where
  true    :: a
  not     :: a -> a
  toBool  :: a -> Bool

instance Boolean Bool where
  true = True
  not = Prelude.not
  toBool = id

instance Boolean (Data Int) where
  true = (MkData 1)
  not x = if x == (MkData 0) then (MkData 1) else (MkData 0)
  toBool x = (x /= 0)

instance Semigroup (Data Int) where
  (<>) = (+)

instance Monoid (Data Int) where
  mempty = 0

-- | We now consider a value to be a numeric monoid which could also be converted
--   into booleans
type Value a = (Eq a, Num a, Boolean a, Monoid a)
