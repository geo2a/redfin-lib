{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
 Module     : ISA.Types
 Copyright  : (c) Georgy Lukyanov 2019
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Types describing the ISA
-}
module ISA.Types (
    -- * Types describing architecture concepts

    -- ** data register
    Register (..),

    -- ** concrete memory location
    CAddress (..),

    -- ** flag
    Flag (..),

    -- ** immediate argument
    Imm (..),

    -- ** instruction code
    InstructionCode (..),

    -- * Classes abstracting values that the ISA model can operate with
    Value,
    ToValue (..),

    -- * bitvector-to-binary expansion/compression
    blastLE,
    fromBitsLE,
    fromBitsLEInt8,
    fromBitsLEInt32,
    fromBitsLEWord8,
    fromBitsLEWord16,
    pad,
) where

import Control.DeepSeq
import Control.Monad
import Data.Aeson hiding (Value)
import Data.Bits
import Data.Bool
import Data.Int (Int32, Int8)
import Data.Monoid
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Generic.Random
import Test.QuickCheck (Arbitrary, arbitrary)
import Prelude hiding (not)

import ISA.Types.Boolean

-- | Data registers
data Register = R0 | R1 | R2 | R3
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Register where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Register

instance NFData Register

instance Arbitrary Register where
    arbitrary = genericArbitrary uniform

-- | Memory location
newtype CAddress = CAddress Word8
    deriving
        ( Eq
        , Ord
        , Num
        , Real
        , Enum
        , Integral
        , Bounded
        , Bits
        , FiniteBits
        , Generic
        , ToJSON
        , FromJSON
        )
    deriving (Show, Read) via Word8

instance NFData CAddress

instance Arbitrary CAddress where
    arbitrary = genericArbitrary uniform

-- | Flag
data Flag
    = Halted
    | Condition
    | Overflow
    | DivisionByZero
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Flag where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Flag

instance NFData Flag

instance Arbitrary Flag where
    arbitrary = genericArbitrary uniform

-- | Immediate argument
newtype Imm a = Imm a
    deriving (Functor, Eq, Ord, Enum, Num, Real, Integral, Bits, FiniteBits, Generic)
    deriving (Show) via a

instance Arbitrary a => Arbitrary (Imm a) where
    arbitrary = genericArbitrary uniform

-- | A binary representation of an instruction
newtype InstructionCode = InstructionCode Word16
    deriving (Eq, Ord, Num, Bits, FiniteBits, Generic)
    deriving (Show) via Word16

instance ToJSON InstructionCode where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InstructionCode

instance Arbitrary InstructionCode where
    arbitrary = genericArbitrary uniform

-- -- | Packaging data in a newtype allows to redefine typeclass instances
-- newtype Data a = MkData { _unData :: a }
--   deriving (Functor, Eq, Ord, Num, Enum, Real, Integral
--            , Typeable, Bounded, Bits, FiniteBits, Generic)
--   deriving Show via a
--   deriving Applicative via Identity

-- instance ToJSON a => ToJSON (Data a) where
--   toEncoding = genericToEncoding defaultOptions
-- instance FromJSON a => FromJSON (Data a) where

-- instance Arbitrary a => Arbitrary (Data a) where
--   arbitrary = genericArbitrary uniform

-----------------------------------------------------------------------------

instance Boolean Int8 where
    toBool x = x /= 0
    fromBool b = if b then 1 else 0
    true = 1
    not x = if x == 0 then 1 else 0

    x ||| y = if toBool x ||| toBool y then 1 else 0
    x &&& y = if toBool x &&& toBool y then 1 else 0

instance Boolean Int32 where
    toBool x = x /= 0
    fromBool b = if b then 1 else 0
    true = 1
    not x = if x == 0 then 1 else 0

    x ||| y = if toBool x ||| toBool y then 1 else 0
    x &&& y = if toBool x &&& toBool y then 1 else 0

-- instance BEq a => BEq (Data a) where
--   (MkData x) === (MkData y) = MkData (x === y)

-- instance BOrd a => BOrd (Data a) where
--   (MkData x) `lt` (MkData y) = MkData (x `lt` y)
--   (MkData x) `gt` (MkData y) = MkData (x `gt` y)

instance BEq Int8 where
    x === y = fromBool $ x == y

instance BEq Int32 where
    x === y = fromBool $ x == y
instance BOrd Int8 where
    lt x y = fromBool $ x < y
    gt x y = fromBool $ x > y

instance BOrd Int32 where
    lt x y = fromBool $ x < y
    gt x y = fromBool $ x > y

instance Semigroup Int8 where
    (<>) = (+)

instance Monoid Int8 where
    mempty = 0

instance Semigroup Int32 where
    (<>) = (+)

instance Monoid Int32 where
    mempty = 0

{- | We now consider a value to be a numeric monoid which could also be converted
   into booleans
-}
class
    (Show a, BEq a, BOrd a, Monoid a, Num a, Integral a, Bounded a, Boolean a) =>
    Value a

instance
    (Show a, BEq a, BOrd a, Monoid a, Num a, Integral a, Bounded a, Boolean a) =>
    Value a

class Value a => ToValue a where
    toValue :: a -> a

instance Value a => ToValue a where
    toValue = id

-----------------------------------------------------------------------------
fromBitsLE :: (FiniteBits a, Num a) => [Bool] -> a
fromBitsLE = go 0 0
  where
    go acc _ [] = acc
    go acc i (x : xs) = go (if x then setBit acc i else acc) (i + 1) xs

fromBitsLEInt32 :: [Bool] -> Int32
fromBitsLEInt32 xs
    | length xs == 32 = fromBitsLE xs
    | otherwise =
        error $
            "ISA.Types.fromBitsLEInt32: "
                <> "the argument does not fit into Int32"

fromBitsLEWord8 :: [Bool] -> Word8
fromBitsLEWord8 xs
    | length xs == 8 = fromBitsLE xs
    | otherwise =
        error $
            "ISA.Types.fromBitsLEWord8: "
                <> "the argument does not fit into Word8"

fromBitsLEWord16 :: [Bool] -> Word16
fromBitsLEWord16 xs
    | length xs == 16 = fromBitsLE xs
    | otherwise =
        error $
            "ISA.Types.fromBitsLEWord16: "
                <> "the argument "
                <> show xs
                <> " does not fit into Word16"

fromBitsLEInt8 :: [Bool] -> Int8
fromBitsLEInt8 xs
    | length xs == 8 = fromBitsLE xs
    | otherwise =
        error $
            "ISA.Types.fromBitsLEInt8: "
                <> "the argument does not fit into Int8"

blastLE :: (FiniteBits a) => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

pad :: Int -> [Bool]
pad k = replicate k False
