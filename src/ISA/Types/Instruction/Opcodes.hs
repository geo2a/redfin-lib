{-# LANGUAGE GADTs #-}

{- |
 Module     : ISA.Types.Instruction.Opcodes
 Copyright  : (c) Georgy Lukyanov 2019
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Opcodes of instructions and related types
-}
module ISA.Types.Instruction.Opcodes (
    -- * Binary representation of an instruction tag
    Opcode (..),
    asBools,

    -- * extract opcode from an instruction code
    opcode,

    -- * numeration type of all opcodes
    InstructionTag (..),
    tag,
) where

import Control.Monad (replicateM)

import ISA.Types.Instruction

-- | Binary representation of an instruction tag
newtype Opcode = MkOpcode [Bool]
    deriving (Eq)

asBools :: Opcode -> [Bool]
asBools (MkOpcode c) = c

-- | Redfin has 6 bit-long opcodes, we stic to that for now
opcode_width :: Int
opcode_width = 6

-- | Print a boolean list as a string of 1s and 0s
showBoolsAsBin :: [Bool] -> String
showBoolsAsBin [] = []
showBoolsAsBin (True : xs) = "1" <> showBoolsAsBin xs
showBoolsAsBin (False : xs) = "0" <> showBoolsAsBin xs

instance Show Opcode where
    show (MkOpcode xs) = "0b" <> showBoolsAsBin xs

{- | Tags of instructions
   It would be cool to derive this type via generics-sop, but
   it's not immediately obvious how to make GHC.Generic to handle
   the 'InstructionImpl' GADT
-}
data InstructionTag where
    TagHalt :: InstructionTag
    TagLoad :: InstructionTag
    TagSet :: InstructionTag
    TagStore :: InstructionTag
    TagAdd :: InstructionTag
    TagAddI :: InstructionTag
    TagSub :: InstructionTag
    TagSubI :: InstructionTag
    TagMul :: InstructionTag
    TagDiv :: InstructionTag
    TagMod :: InstructionTag
    TagAbs :: InstructionTag
    TagJump :: InstructionTag
    TagLoadMI :: InstructionTag
    TagCmpEq :: InstructionTag
    TagCmpGt :: InstructionTag
    TagCmpLt :: InstructionTag
    TagJumpCt :: InstructionTag
    TagJumpCf :: InstructionTag

deriving instance Eq InstructionTag
deriving instance Bounded InstructionTag
deriving instance Enum InstructionTag
deriving instance Show InstructionTag

toTag :: Instruction a -> InstructionTag
toTag (Instruction i) =
    case i of
        Halt -> TagHalt
        Load _ _ -> TagLoad
        Set _ _ -> TagSet
        Store _ _ -> TagStore
        Add _ _ -> TagAdd
        AddI _ _ -> TagAddI
        Sub _ _ -> TagSub
        SubI _ _ -> TagSubI
        Mul _ _ -> TagMul
        Div _ _ -> TagDiv
        Mod _ _ -> TagMod
        Abs _ -> TagAbs
        Jump _ -> TagJump
        JumpCt _ -> TagJumpCt
        JumpCf _ -> TagJumpCf
        LoadMI _ _ -> TagLoadMI
        CmpEq _ _ -> TagCmpEq
        CmpGt _ _ -> TagCmpGt
        CmpLt _ _ -> TagCmpLt

-- | Generate a list of all possible opcodes of the specified width
enumOpcodes :: Int -> [Opcode]
enumOpcodes width =
    map MkOpcode $ replicateM width [False, True]

-- | Calculate the opcode of an instruction
opcode :: Show a => Instruction a -> Opcode
opcode i =
    case lookup (toTag i) (zip [minBound .. maxBound] (enumOpcodes opcode_width)) of
        Just c -> c
        Nothing -> error $ "ISA.Types.Instruction.Opcodes.opcode: can't generate opcode for instruction" <> show i

-- | Calculate the instruction tag of an opcode
tag :: Opcode -> Maybe InstructionTag
tag code =
    lookup code (zip (enumOpcodes opcode_width) [minBound .. maxBound])
