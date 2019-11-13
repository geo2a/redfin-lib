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
import           Data.Int              (Int32)
import           Data.Word             (Word32)

import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Symbolic

concretiseInstr :: Instruction (Data Sym) -> Instruction (Data Int32)
concretiseInstr = \case
    Instruction Halt -> Instruction  Halt
    Instruction (Load     r addr) -> Instruction (Load r addr)
    Instruction (LoadMI   r addr) -> Instruction (LoadMI r addr)
    Instruction (Set      r (Imm (MkData byte))) ->
      case toImm byte of
        Right imm -> Instruction (Set r imm)
        Left sym -> error $ "Instruction.decode: symbolic instruction "
                         <> show sym
    Instruction (Store    r addr) -> Instruction (Store r addr)
    Instruction (Add      r addr) -> Instruction (Add r addr)
    Instruction (Jump     (Imm (MkData byte)))   ->
      case toImm byte of
        Right imm -> Instruction (Jump imm)
        Left sym -> error $ "Instruction.decode: symbolic instruction "
                         <> show sym
    Instruction (CmpEq      r addr) -> Instruction (CmpEq r addr)
    Instruction (CmpLt      r addr) -> Instruction (CmpLt r addr)
    Instruction (CmpGt      r addr) -> Instruction (CmpGt r addr)
    Instruction (JumpCt (Imm (MkData byte)))   ->
      case toImm byte of
        Right imm -> Instruction (JumpCt imm)
        Left sym -> error $ "Instruction.decode: symbolic instruction "
                         <> show sym
    Instruction (JumpCf (Imm (MkData byte)))   ->
      case toImm byte of
        Right imm -> Instruction (JumpCf imm)
        Left sym -> error $ "Instruction.decode: symbolic instruction "
                         <> show sym
    Instruction (Sub      r addr) -> Instruction (Sub r addr)
    Instruction (Mul      r addr) -> Instruction (Mul r addr)
    Instruction (Div      r addr) -> Instruction (Div r addr)
    Instruction (Mod      r addr) -> Instruction (Mod r addr)
    Instruction (Abs      r)      -> Instruction (Abs r)
    -- where f = False
    --       t = True
    --       pad k = replicate k f

encode :: Instruction (Data Int32) -> InstructionCode
encode = \case
    Instruction Halt -> 0
    Instruction (Load     r addr) ->
        fromBitsLE $ [f, f, f, f, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (LoadMI   r addr) ->
        fromBitsLE $ [f, f, f, f, t, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (Set      r (Imm (MkData byte))) ->
        fromBitsLE $ [f, f, f, f, t, t] ++ encodeRegister r
                                        ++ encodeByte (Imm byte)
                                        ++ pad 8
    Instruction (Store    r addr) ->
        fromBitsLE $ [f, f, f, t, f, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (Add      r addr) ->
        fromBitsLE $ [f, f, f, t, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (Jump     (Imm (MkData byte)))   ->
        fromBitsLE $ [f, f, f, t, t, f] ++ encodeByte (Imm byte)
                                        ++ pad 18
    Instruction (CmpEq      r addr) ->
        fromBitsLE $ [f, t, f, f, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (CmpLt      r addr) ->
        fromBitsLE $ [f, t, f, f, t, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (CmpGt      r addr) ->
        fromBitsLE $ [f, t, f, f, t, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (JumpCt (Imm (MkData byte)))   ->
        fromBitsLE $ [t, t, f, f, f, t] ++ encodeByte (Imm byte)
                                        ++ pad 18
    Instruction (JumpCf (Imm (MkData byte)))   ->
        fromBitsLE $ [t, t, f, f, t, f] ++ encodeByte (Imm byte)
                                        ++ pad 18
    Instruction (Sub      r addr) ->
        fromBitsLE $ [f, f, t, f, f, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (Mul      r addr) ->
        fromBitsLE $ [f, f, t, f, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (Div      r addr) ->
        fromBitsLE $ [f, f, t, f, t, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (Mod      r addr) ->
        fromBitsLE $ [f, f, t, f, t, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 8
    Instruction (Abs      r)      ->
        fromBitsLE $ [f, f, t, t, f, f] ++ encodeRegister r
                                        ++ pad 24
    where f = False
          t = True
          pad k = replicate k f

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
