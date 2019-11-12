{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Instruction.Decode
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Instruction decoder
--
-----------------------------------------------------------------------------

module ISA.Types.Instruction.Decode
    (decode) where

import           Data.Bits
import           Data.Int              (Int32)

import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Symbolic

decode :: InstructionCode (Data Int32) -> Maybe (Instruction (Data Int32))
decode (InstructionCode code) =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [f, f, f, f, f, f]  -> Just $ Instruction Halt
          | opcode == [f, f, f, f, f, t]  -> Just $ Instruction $
                Load (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, f, t, f]   -> Just $ Instruction $
                LoadMI (decodeRegister . extractRegister $ expandedCode)
                       (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, f, t, t]  -> Just $ Instruction $
                Set (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractSImm8 expandedCode)
          | opcode == [f, f, f, t, f, f]   -> Just $ Instruction $
                Store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, f, t]   -> Just $ Instruction $
                Add (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, t, f]   -> Just $ Instruction $
                Jump (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [f, t, f, f, f, t]   -> Just $ Instruction $
                CmpEq (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, t, f, f, t, f]   -> Just $ Instruction $
                CmpLt (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, t, f, f, t, t]   -> Just $ Instruction $
                CmpGt (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [t, t, f, f, f, t]    -> Just $ Instruction $
                JumpCt (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [t, t, f, f, t, f]    -> Just $ Instruction $
                JumpCf (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [f, f, t, f, f, f]   -> Just $ Instruction $
                Sub (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, f, t]   -> Just $ Instruction $
                Mul (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, t, f]   -> Just $ Instruction $
                Div (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, t, t]   -> Just $ Instruction $
                Mod (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, t, f, f]   -> Just $ Instruction $
                Abs (decodeRegister . extractRegister $ expandedCode)
          | otherwise -> Nothing
      where f = False
            t = True

fromBitsLE :: (FiniteBits a, Num a) => [Bool] -> a
fromBitsLE = go 0 0
  where go acc _  []    = acc
        go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs

blastLE :: FiniteBits a => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

decodeRegister :: [Bool] -> Register
decodeRegister = \case
      [False, False] -> R0
      [False, True]  -> R1
      [True, False]  -> R2
      [True, True]   -> R3
      _              -> error $ "Machine.Instruction.Decode.decodeRegister:"
                             <> "register must be encoded as a two-bit word"

decodeOpcode :: [Bool] -> [Bool]
decodeOpcode = take 6

extractRegister :: [Bool] -> [Bool]
extractRegister = take 2 . drop 6

extractMemoryAddress :: [Bool] -> [Bool]
extractMemoryAddress = (++ pad 56) . take 8 . drop 8

extractSImm8 :: [Bool] -> [Bool]
extractSImm8 = (++ pad 56) . take 8 . drop 8

extractSImm8Jump :: [Bool] -> [Bool]
extractSImm8Jump = (++ pad 56) . take 8 . drop 6

pad :: Int -> [Bool]
pad k = replicate k False
