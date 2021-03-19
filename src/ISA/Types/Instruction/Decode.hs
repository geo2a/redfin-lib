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
symbolise :: Instruction Int32 -> Instruction Sym
symbolise (Instruction i) =
  case i of
    Halt              -> mkI $ Halt @Value
    Load   reg1 addr1 -> mkI $ Load @Value   reg1 addr1
    Add    reg1 addr1 -> mkI $ Add @Value    reg1 addr1
    AddI   reg1 imm   -> mkI $ AddI @Value   reg1 (SConst . CInt32 <$> imm)
    Sub    reg1 addr1 -> mkI $ Sub @Value    reg1 addr1
    SubI   reg1 imm   -> mkI $ SubI @Value   reg1 (SConst . CInt32 <$> imm)
    Mul    reg1 addr1 -> mkI $ Mul @Value    reg1 addr1
    Div    reg1 addr1 -> mkI $ Div @Value    reg1 addr1
    Mod    reg1 addr1 -> mkI $ Mod @Value    reg1 addr1
    Store  reg1 addr1 -> mkI $ Store @Value  reg1 addr1
    Set    reg1 imm   -> mkI $ Set @Value    reg1 (SConst . CInt32 <$> imm)
    Abs    reg1       -> mkI $ Abs @Value    reg1
    Jump   offset1    -> mkI $ Jump @Value   (SConst . CInt32 <$> offset1)
    JumpCt offset1    -> mkI $ JumpCt @Value (SConst . CInt32 <$> offset1)
    JumpCf offset1    -> mkI $ JumpCf @Value (SConst . CInt32 <$> offset1)
    LoadMI reg1 addr1 -> mkI $ LoadMI @Value reg1 addr1
    CmpEq  reg1 addr1 -> mkI $ CmpEq @Value  reg1 addr1
    CmpGt  reg1 addr1 -> mkI $ CmpGt @Value  reg1 addr1
    CmpLt  reg1 addr1 -> mkI $ CmpLt @Value  reg1 addr1

-- | Try to decode the instruction represented by a concrete symbolic instruction code
toInstruction :: Sym -> Either Sym (Instruction Int32)
toInstruction sym = case sym of
  (SConst (CWord ic)) -> case decode (InstructionCode ic) of
                           Just i  -> Right i
                           Nothing -> Left sym
  _                   -> Left sym

decode :: Value a => InstructionCode -> Maybe (Instruction a)
decode (InstructionCode code) =
    let expandedCode = blastLE code
        opcode = decodeOpcode expandedCode
    in case tag opcode of
      Just TagHalt   -> Just $ Instruction (Halt @Value)
      Just TagLoad   -> Just $ Instruction $
              Load @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagSet    -> Just $ Instruction $
              Set @Value (decodeRegister . extractRegister $ expandedCode)
                  (Imm . fromIntegral . fromBitsLEInt8 $ extractSImm8 expandedCode)
      Just TagStore  -> Just $ Instruction $
              Store @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagAdd    -> Just $ Instruction $
              Add @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagAddI   -> Just $ Instruction $
              AddI @Value (decodeRegister . extractRegister $ expandedCode)
             (Imm . fromIntegral . fromBitsLEInt8 $ extractSImm8 expandedCode)
      Just TagSub    -> Just $ Instruction $
              Sub @Value (decodeRegister . extractRegister $ expandedCode)
                  (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagSubI   -> Just $ Instruction $
              SubI @Value (decodeRegister . extractRegister $ expandedCode)
             (Imm . fromIntegral . fromBitsLEInt8 $ extractSImm8 expandedCode)
      Just TagMul    -> Just $ Instruction $
              Mul @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagDiv    -> Just $ Instruction $
              Div @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagMod    -> Just $ Instruction $
              Mod @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagAbs    -> Just $ Instruction $
              Abs @Value (decodeRegister . extractRegister $ expandedCode)
      Just TagJump   -> Just $ Instruction $
              Jump @Value (Imm . fromIntegral . fromBitsLEInt8
                    $ extractSImm8Jump expandedCode)
      Just TagLoadMI -> Just $ Instruction $
              LoadMI @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagCmpEq  -> Just $ Instruction $
              CmpEq @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagCmpGt  -> Just $ Instruction $
              CmpGt @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagCmpLt  -> Just $ Instruction $
              CmpLt @Value (decodeRegister . extractRegister $ expandedCode)
                   (CAddress . fromBitsLEWord8 $ extractMemoryAddress expandedCode)
      Just TagJumpCt -> Just $ Instruction $
              JumpCt @Value
             (Imm . fromIntegral . fromBitsLEInt8 $ extractSImm8Jump expandedCode)
      Just TagJumpCf -> Just $ Instruction $
              JumpCf @Value
             (Imm . fromIntegral . fromBitsLEInt8 $ extractSImm8Jump expandedCode)
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
extractMemoryAddress = take 16 . drop 8

extractSImm8 :: [Bool] -> [Bool]
extractSImm8 = take 8 . drop 8

extractSImm8Jump :: [Bool] -> [Bool]
extractSImm8Jump = take 8 . drop 6
