{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
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
    -- * a typeclass representing things that could be converted to a memory location
    , Addressable(..)
      -- ** flag
    , Flag (..)
      -- immediate argument
    , Imm (..)
    -- instruction code
    , InstructionCode (..)
    -- packaged data
    , Data (..)
    -- equality check that may fail
    , Prop(..), TryEq (..), TryOrd(..)
    -- * Abstraction over possible locations in the ISA
    , Key(..), keyTag

    -- * Classes abstracting values that ISA model can operate with
    -- ** Booleans
    , Boolean (..)
    , Value
    , blastLE, fromBitsLE, pad
    ) where

import           Data.Bits
import           Data.Int        (Int32)
import           Data.Typeable
import           Data.Word       (Word16, Word32)
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
  deriving (Eq, Ord, Num, Real, Enum, Integral, Bounded, Bits, FiniteBits, Generic)
  deriving Show via Word16

instance Arbitrary Address where
  arbitrary = genericArbitrary uniform

-- | Flag
data Flag = Halted
          | Zero
          | Condition
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary Flag where
  arbitrary = genericArbitrary uniform

-- | Immediate argument
newtype Imm a = Imm a
  deriving (Functor, Eq, Num, Bits, FiniteBits, Generic)
  deriving Show via a

instance Arbitrary a => Arbitrary (Imm a) where
  arbitrary = genericArbitrary uniform

-- | A binary representation of an instruction
newtype InstructionCode = InstructionCode Word16
  deriving (Eq, Ord, Num, Bits, FiniteBits, Generic)
  deriving Show via Word16

instance Arbitrary InstructionCode where
  arbitrary = genericArbitrary uniform

newtype Data a = MkData a
  deriving (Functor, Eq, Ord, Num, Typeable, Bounded, Bits, FiniteBits, Generic)
  deriving Show via a

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

keyTag :: Key -> String
keyTag = \case
  Reg  _ -> "register"
  Addr _ -> "address"
  F    _ -> "flag"
  IC     -> "instruction-counter"
  IR     -> "instruction-register"
  Prog _ -> "program-address"

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


data Prop a = Trivial Bool
            | Nontrivial a
            deriving (Show, Typeable)

instance Boolean (Prop a) where
  true = Trivial True
  not  = error "Prop.Boolean.not: not is undefined"
  toBool t = case t of
    Trivial b    -> b
    Nontrivial _ -> True
  (|||) = error "Prop.Boolean.|||: undefined"
  (&&&) = error "Prop.Boolean.&&&: undefined"

-- | This class abstracts an equality check with possible failure, i.e. in the
--   case when the values are symbolic. In case of concrete types with an 'Eq'
--   instance '(===)' will always return @Trivial@.
class TryEq a where
  (===) :: a -> a -> Prop a

-- instance Eq a => TryEq (Data a) where
--   (MkData x) === (MkData y) = Trivial (x == y)

instance TryEq (Data Int32) where
  (MkData x) === (MkData y) = Trivial (x == y)

-- | Similar for TryEq, but for strict order
class TryOrd a where
  lt :: a -> a -> Prop a
  gt :: a -> a -> Prop a

class Addressable a where
  toMemoryAddress :: a -> Maybe Address

instance Addressable (Data Int32) where
  toMemoryAddress x | x < 0 = Nothing
                    | x >= fromIntegral (maxBound :: Address) = Nothing
                    | otherwise = Just . fromBitsLE . extractMemoryAddress . blastLE $ x

instance TryOrd (Data Int32) where
  lt (MkData x) (MkData y) = Trivial (x < y)
  gt (MkData x) (MkData y) = Trivial (x > y)

instance Semigroup (Data Int32) where
  (<>) = (+)

instance Monoid (Data Int32) where
  mempty = 0

-- | We now consider a value to be a numeric monoid which could also be converted
--   into booleans
type Value a = (Show a, TryEq a, TryOrd a, Monoid a, Num a, Boolean a)
-----------------------------------------------------------------------------
fromBitsLE :: (FiniteBits a, Num a) => [Bool] -> a
fromBitsLE = go 0 0
  where go acc _  []    = acc
        go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs

blastLE :: FiniteBits a => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

pad :: Int -> [Bool]
pad k = replicate k False

extractMemoryAddress :: [Bool] -> [Bool]
extractMemoryAddress = (++ pad 24) . take 8 . drop 8
