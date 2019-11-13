{-# LANGUAGE GADTs      #-}
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
    (decode, symbolise, toInstruction) where

import           Data.Bits
import           Data.Int              (Int32)

import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Symbolic

symbolise :: Instruction (Data Int32) -> Instruction (Data Sym)
symbolise (Instruction i) =
  case i of
    Halt              -> mkI $ Halt
    Load   reg1 addr1 -> mkI $ Load   reg1 addr1
    Add    reg1 addr1 -> mkI $ Add    reg1 addr1
    Sub    reg1 addr1 -> mkI $ Sub    reg1 addr1
    Mul    reg1 addr1 -> mkI $ Mul    reg1 addr1
    Div    reg1 addr1 -> mkI $ Div    reg1 addr1
    Mod    reg1 addr1 -> mkI $ Mod    reg1 addr1
    Store  reg1 addr1 -> mkI $ Store  reg1 addr1
    Set    reg1 imm1  -> mkI $ Set    reg1 ((fmap (SConst . CInt)) <$> imm1)
    Abs    reg1       -> mkI $ Abs    reg1
    Jump   offset1    -> mkI $ Jump   ((fmap (SConst . CInt)) <$> offset1)
    JumpCt offset1    -> mkI $ JumpCt ((fmap (SConst . CInt)) <$> offset1)
    JumpCf offset1    -> mkI $ JumpCf ((fmap (SConst . CInt)) <$> offset1)
    LoadMI reg1 addr1 -> mkI $ LoadMI reg1 addr1
    CmpEq  reg1 addr1 -> mkI $ CmpEq  reg1 addr1
    CmpGt  reg1 addr1 -> mkI $ CmpGt  reg1 addr1
    CmpLt  reg1 addr1 -> mkI $ CmpLt  reg1 addr1


toInstruction :: Sym -> Either Sym (Instruction (Data Int32))
toInstruction sym = case sym of
  (SConst (CWord ic)) -> case decode (InstructionCode ic) of
                           Just i  -> Right i
                           Nothing -> Left sym
  _                   -> Left sym

decode :: InstructionCode -> Maybe (Instruction (Data Int32))
decode (InstructionCode code) =
    let expandedCode = blastLE code
        opcode = decodeOpcode expandedCode
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
extractMemoryAddress = (++ pad 24) . take 8 . drop 8

extractSImm8 :: [Bool] -> [Bool]
extractSImm8 = (++ pad 24) . take 8 . drop 8

extractSImm8Jump :: [Bool] -> [Bool]
extractSImm8Jump = (++ pad 24) . take 8 . drop 6

pad :: Int -> [Bool]
pad k = replicate k False
