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
    ( -- * decode an instruction from a binary instruction code
      decode
      -- * covert a concrete instruction into a symbolic one
    , symbolise
      -- * try to decode the instruction represented by a concrete symbolic instruction code
    , toInstruction) where

import           Data.Int                      (Int32)

import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Opcodes
import           ISA.Types.Symbolic

-- | Covert a concrete instruction into a symbolic one
symbolise :: Instruction (Data Int32) -> Instruction (Data Sym)
symbolise (Instruction i) =
  case i of
    Halt              -> mkI $ Halt
    Load   reg1 addr1 -> mkI $ Load   reg1 addr1
    Add    reg1 addr1 -> mkI $ Add    reg1 addr1
    AddI   reg1 imm   -> mkI $ AddI   reg1 ((fmap (SConst . CInt32)) <$> imm)
    Sub    reg1 addr1 -> mkI $ Sub    reg1 addr1
    SubI   reg1 imm   -> mkI $ SubI   reg1 ((fmap (SConst . CInt32)) <$> imm)
    Mul    reg1 addr1 -> mkI $ Mul    reg1 addr1
    Div    reg1 addr1 -> mkI $ Div    reg1 addr1
    Mod    reg1 addr1 -> mkI $ Mod    reg1 addr1
    Store  reg1 addr1 -> mkI $ Store  reg1 addr1
    Set    reg1 imm1  -> mkI $ Set    reg1 ((fmap (SConst . CInt32)) <$> imm1)
    Abs    reg1       -> mkI $ Abs    reg1
    Jump   offset1    -> mkI $ Jump   ((fmap (SConst . CInt32)) <$> offset1)
    JumpCt offset1    -> mkI $ JumpCt ((fmap (SConst . CInt32)) <$> offset1)
    JumpCf offset1    -> mkI $ JumpCf ((fmap (SConst . CInt32)) <$> offset1)
    LoadMI reg1 addr1 -> mkI $ LoadMI reg1 addr1
    CmpEq  reg1 addr1 -> mkI $ CmpEq  reg1 addr1
    CmpGt  reg1 addr1 -> mkI $ CmpGt  reg1 addr1
    CmpLt  reg1 addr1 -> mkI $ CmpLt  reg1 addr1

-- | Try to decode the instruction represented by a concrete symbolic instruction code
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
    in case tag opcode of
      Just TagHalt   -> Just $ Instruction Halt
      Just TagLoad   -> Just $ Instruction $
              Load (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagSet    -> Just $ Instruction $
              Set (decodeRegister . extractRegister $ expandedCode)
                  (Imm . MkData . fromIntegral . fromBitsLEInt8 $ extractSImm8 expandedCode)
      Just TagStore  -> Just $ Instruction $
              Store (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagAdd    -> Just $ Instruction $
              Add (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagAddI   -> Just $ Instruction $
              AddI (decodeRegister . extractRegister $ expandedCode)
             (Imm . MkData . fromIntegral . fromBitsLEInt8 $ extractSImm8 expandedCode)
      Just TagSub    -> Just $ Instruction $
              Sub (decodeRegister . extractRegister $ expandedCode)
                  (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagSubI   -> Just $ Instruction $
              SubI (decodeRegister . extractRegister $ expandedCode)
             (Imm . MkData . fromIntegral . fromBitsLEInt8 $ extractSImm8 expandedCode)
      Just TagMul    -> Just $ Instruction $
              Mul (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagDiv    -> Just $ Instruction $
              Div (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagMod    -> Just $ Instruction $
              Mod (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagAbs    -> Just $ Instruction $
              Abs (decodeRegister . extractRegister $ expandedCode)
      Just TagJump   -> Just $ Instruction $
              Jump (Imm . MkData . fromIntegral . fromBitsLEInt8
                    $ extractSImm8Jump expandedCode)
      Just TagLoadMI -> Just $ Instruction $
              LoadMI (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagCmpEq  -> Just $ Instruction $
              CmpEq (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagCmpGt  -> Just $ Instruction $
              CmpGt (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagCmpLt  -> Just $ Instruction $
              CmpLt (decodeRegister . extractRegister $ expandedCode)
                   (Address . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagJumpCt -> Just $ Instruction $
              JumpCt
             (Imm . MkData . fromIntegral . fromBitsLEInt8 $ extractSImm8Jump expandedCode)
      Just TagJumpCf -> Just $ Instruction $
              JumpCf
             (Imm . MkData . fromIntegral . fromBitsLEInt8 $ extractSImm8Jump expandedCode)
      Nothing -> Nothing

decodeRegister :: [Bool] -> Register
decodeRegister = \case
      [False, False] -> R0
      [False, True]  -> R1
      [True, False]  -> R2
      [True, True]   -> R3
      _              -> error $ "Machine.Instruction.Decode.decodeRegister:"
                             <> "register must be encoded as a two-bit word"

decodeOpcode :: [Bool] -> Opcode
decodeOpcode = MkOpcode . take 6

extractRegister :: [Bool] -> [Bool]
extractRegister = take 2 . drop 6

extractMemoryAddress :: [Bool] -> [Bool]
extractMemoryAddress = take 8 . drop 8

extractSImm8 :: [Bool] -> [Bool]
extractSImm8 = take 8 . drop 8

extractSImm8Jump :: [Bool] -> [Bool]
extractSImm8Jump = take 8 . drop 6
