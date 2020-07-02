{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Instruction.Encode
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Instruction encoder
--
-----------------------------------------------------------------------------

module ISA.Types.Instruction.Encode
    (encode
    , concretiseInstr) where

import           Data.Int                      (Int32, Int8)

import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Opcodes
import           ISA.Types.Symbolic

{-# WARNING concretiseImm "Throws a runtime exception is the argument is symbolic" #-}
concretiseImm :: Imm (Data Sym) -> Imm (Data Int8)
concretiseImm (Imm (MkData i)) =
  case toImm i of
    Right imm -> imm
    Left sym -> error $ "Instruction.encode: symbolic immediate argument"
                <> show sym

concretiseInstr :: Instruction (Data Sym) -> Instruction (Data Int8)
concretiseInstr = \case
    Instruction Halt -> Instruction  Halt
    Instruction (Load     r addr) -> Instruction (Load r addr)
    Instruction (LoadMI   r addr) -> Instruction (LoadMI r addr)
    Instruction (Set      r imm) ->  Instruction (Set r (concretiseImm imm))
    Instruction (Store    r addr) -> Instruction (Store r addr)
    Instruction (Add      r addr) -> Instruction (Add r addr)
    Instruction (AddI     r imm)  -> Instruction (AddI r (concretiseImm imm))
    Instruction (Jump     imm   ) -> Instruction (Jump (concretiseImm imm))
    Instruction (CmpEq      r addr) -> Instruction (CmpEq r addr)
    Instruction (CmpLt      r addr) -> Instruction (CmpLt r addr)
    Instruction (CmpGt      r addr) -> Instruction (CmpGt r addr)
    Instruction (JumpCt imm)   -> Instruction (JumpCt (concretiseImm imm))
    Instruction (JumpCf imm)   -> Instruction (JumpCf (concretiseImm imm))
    Instruction (Sub      r addr) -> Instruction (Sub r addr)
    Instruction (SubI     r imm)  -> Instruction (SubI r (concretiseImm imm))
    Instruction (Mul      r addr) -> Instruction (Mul r addr)
    Instruction (Div      r addr) -> Instruction (Div r addr)
    Instruction (Mod      r addr) -> Instruction (Mod r addr)
    Instruction (Abs      r)      -> Instruction (Abs r)

encode :: Instruction (Data Int32) -> InstructionCode
encode i = InstructionCode . fromBitsLEWord16 $ case i of
    Instruction Halt -> take 16 $ repeat False
    Instruction (Load     r addr) -> (asBools . opcode $ i)
                                     <> encodeRegister r
                                     <> encodeMemoryAddress addr
    Instruction (LoadMI   r addr) -> (asBools . opcode $ i)
                                     <> encodeRegister r
                                     <> encodeMemoryAddress addr
    Instruction (Set      r (Imm (MkData byte))) -> (asBools . opcode $ i)
                                                    <> encodeRegister r
                                                    <> encodeByte (Imm byte)
    Instruction (Store    r addr) -> (asBools . opcode $ i)
                                     <> encodeRegister r
                                     <> encodeMemoryAddress addr
    Instruction (Add      r addr) -> (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
    Instruction (AddI     r (Imm (MkData byte))) -> (asBools . opcode $ i)
                                                 <> encodeRegister r
                                            ++ encodeByte (Imm byte)
    Instruction (Jump     (Imm (MkData byte)))   -> (asBools . opcode $ i)
                                                 <> encodeByte (Imm byte)
                                                 <> pad 2
    Instruction (CmpEq      r addr) -> (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
    Instruction (CmpLt      r addr) -> (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
    Instruction (CmpGt      r addr) -> (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
    Instruction (JumpCt (Imm (MkData byte)))   ->
        (asBools . opcode $ i) ++ encodeByte (Imm byte)
                                            ++ pad 2
    Instruction (JumpCf (Imm (MkData byte)))   ->
        (asBools . opcode $ i) ++ encodeByte (Imm byte)
                                            ++ pad 2
    Instruction (Sub      r addr) ->
        (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
    Instruction (SubI     r (Imm (MkData byte))) ->
        (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeByte (Imm byte)
    Instruction (Mul      r addr) ->
        (asBools . opcode $ i) ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
    Instruction (Div      r addr) ->
        (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
    Instruction (Mod      r addr) ->
        (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
    Instruction (Abs      r)      ->
        (asBools . opcode $ i) ++ encodeRegister r
                                            ++ pad 8

-- | 'Register' is encoded as a 2-bit word
encodeRegister :: Register -> [Bool]
encodeRegister = \case
    R0 -> [False, False]
    R1 -> [False, True]
    R2 -> [True, False]
    R3 -> [True, True]

-- | 'MemoryAddress' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeMemoryAddress :: Address -> [Bool]
encodeMemoryAddress = blastLE

-- | 'Byte' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeByte :: Imm Int32 -> [Bool]
encodeByte x | x >= fromIntegral (minBound :: Int8)
            && x <= fromIntegral (maxBound :: Int8) =
  blastLE (fromIntegral x :: Int8)
             | otherwise = error $ "encodeByte: doesn't fit into Int8"
