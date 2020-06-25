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
    (encode) where

import           Data.Bits
import           Data.Int                      (Int32)

import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Opcodes
import           ISA.Types.Symbolic

{-# WARNING concretiseImm "Throws a runtime exception is the argument is symbolic" #-}
concretiseImm :: Imm (Data Sym) -> Imm (Data Int32)
concretiseImm (Imm (MkData byte)) =
  case toImm byte of
    Right imm -> imm
    Left sym -> error $ "Instruction.encode: symbolic immediate argument"
                <> show sym

concretiseInstr :: Instruction (Data Sym) -> Instruction (Data Int32)
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
    Instruction (Mul      r addr) -> Instruction (Mul r addr)
    Instruction (Div      r addr) -> Instruction (Div r addr)
    Instruction (Mod      r addr) -> Instruction (Mod r addr)
    Instruction (Abs      r)      -> Instruction (Abs r)

encode :: Instruction (Data Int32) -> InstructionCode
encode i = case i of
    Instruction Halt -> 0
    Instruction (Load     r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (LoadMI   r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (Set      r (Imm (MkData byte))) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeByte (Imm byte)
                                            ++ pad 8
    Instruction (Store    r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (Add      r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (AddI     r (Imm (MkData byte))) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeByte (Imm byte)
                                            ++ pad 8
    Instruction (Jump     (Imm (MkData byte)))   ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeByte (Imm byte)
                                            ++ pad 18
    Instruction (CmpEq      r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (CmpLt      r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (CmpGt      r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (JumpCt (Imm (MkData byte)))   ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeByte (Imm byte)
                                            ++ pad 18
    Instruction (JumpCf (Imm (MkData byte)))   ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeByte (Imm byte)
                                            ++ pad 18
    Instruction (Sub      r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (Mul      r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (Div      r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (Mod      r addr) ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ encodeMemoryAddress addr
                                            ++ pad 8
    Instruction (Abs      r)      ->
        fromBitsLE $ (asBools . opcode $ i) ++ encodeRegister r
                                            ++ pad 24
    where pad k = replicate k False

fromBitsLE :: (FiniteBits a, Num a) => [Bool] -> a
fromBitsLE = go 0 0
  where go acc _  []    = acc
        go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs

blastLE :: FiniteBits a => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

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
encodeByte = take 8 . blastLE
