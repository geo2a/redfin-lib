-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Assembly
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Shallowly-embedded assembly language.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ISA.Assembly where

import           Control.Monad.State
import           Data.Int                     (Int32)
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as Text
import           Debug.Trace                  (trace)

import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Encode
import           ISA.Types.Symbolic

decIfNeg :: Integral a => a -> a
decIfNeg x | x < 0     = x - 1
           | otherwise = x

type Label = Text.Text

-- | TODO: refactor into a bounded type
progAddressSpace :: Int32
progAddressSpace = 256

-- | Unconditional go to a label
goto :: Label -> Script
goto name = do
    s <- get
    here <- instructionCounter <$> get
    case Map.lookup name (labels s) of
         Nothing -> jmpi 0
           -- error $ "ISA.Assembly.goto: no such label " <> show name-- jmpi 0
         Just there -> do
             let offset  =
                   ((fromIntegral there :: Int32) - (fromIntegral here :: Int32) - 1)
             jmpi (fromIntegral offset)

-- | Go to a label
goto_ct :: Label -> Script
goto_ct name = do
    s <- get
    here <- instructionCounter <$> get
    case Map.lookup name (labels s) of
         Nothing -> jmpi 0
         Just there -> do
             let offset  =
                   ((fromIntegral there :: Int32) - (fromIntegral here :: Int32) - 1)
             jmpi_ct (fromIntegral offset)

goto_cf :: Label -> Script
goto_cf name = do
    s <- get
    here <- instructionCounter <$> get
    case Map.lookup name (labels s) of
         Nothing -> jmpi 0
         Just there -> do
             let offset  =
                   ((fromIntegral there :: Int32) - (fromIntegral here :: Int32) - 1)
             jmpi_cf (fromIntegral offset)

type Labels = Map.Map Label Address

data AssemblerState =
    MkAssemblerState { program            :: [(Address, Instruction (Data Int32))]
                     , labels             :: Labels
                     , instructionCounter :: Address
                     }

type Script = State AssemblerState ()

collectLabels :: Script -> Labels
collectLabels src =
    labels $ snd $ runState src (MkAssemblerState [] Map.empty 0)

assemble :: Script -> [(Address, Instruction (Data Int32))]
assemble src =
    prg
  where
    prg = reverse $ program $ snd $ runState src (MkAssemblerState [] labels 0)
    labels = collectLabels src

mkProgram :: Script -> [(Key, Sym)]
mkProgram src =
  let prog = assemble src
      addrs = map Prog [0..]
      ics   = [ SConst (CWord ic) | (InstructionCode ic) <- map (encode . snd) prog]
  in zip addrs ics

instr :: Instruction (Data Int32) -> Script
instr i = do
    s <- get
    let ic = instructionCounter s
    put $ s {program = (ic, i):program s, instructionCounter = ic + 1}

(@@) :: Label -> Script -> Script
name @@ src = do
    label name
    src

label :: Label -> Script
label name = do
    s <- get
    let ic = instructionCounter s
    put $ s {labels = Map.insert name ic $ labels s}

-- Instruction mnemonics
add   rX dmemaddr = instr (Instruction $ Add    rX dmemaddr)
add_i rX imm      = instr (Instruction $ AddI   rX imm)
sub   rX dmemaddr = instr (Instruction $ Sub    rX dmemaddr)
sub_i rX imm      = instr (Instruction $ SubI   rX imm)
mul   rX dmemaddr = instr (Instruction $ Mul    rX dmemaddr)
div   rX dmemaddr = instr (Instruction $ Div    rX dmemaddr)
mod   rX dmemaddr = instr (Instruction $ Mod    rX dmemaddr)
ld    rX dmemaddr = instr (Instruction $ Load   rX dmemaddr)
st    rX dmemaddr = instr (Instruction $ Store  rX dmemaddr)
ldmi  rX dmemaddr = instr (Instruction $ LoadMI rX dmemaddr)
-- stmi  rX dmemaddr = instr (Instruction StoreMI $ rX dmemaddr)
cmpeq rX dmemaddr = instr (Instruction $ CmpEq rX dmemaddr)
cmplt rX dmemaddr = instr (Instruction $ CmpLt rX dmemaddr)
cmpgt rX dmemaddr = instr (Instruction $ CmpGt rX dmemaddr)
-- sl    rX dmemaddr = instr (Instruction $ rX dmemaddr)
-- sr    rX dmemaddr = instr (Instruction $ rX dmemaddr)
-- sra   rX dmemaddr = instr (Instruction $ rX dmemaddr)

-- add_si rX simm = write 0b100000 (register rX .|. simm8 simm)
-- sub_si rX simm = write 0b100001 (register rX .|. simm8 simm)
-- mul_si rX simm = write 0b100010 (register rX .|. simm8 simm)
-- div_si rX simm = write 0b100011 (register rX .|. simm8 simm)
ld_i  rX simm = instr (Instruction $ Set rX simm)

-- sl_i   rX uimm = write 0b101100 (register rX .|. uimm8 uimm)
-- sr_i   rX uimm = write 0b101101 (register rX .|. uimm8 uimm)
-- sra_i  rX uimm = write 0b101110 (register rX .|. uimm8 uimm)
-- ld_i   rX uimm = write 0b101111 (register rX .|. uimm8 uimm)

jmpi    simm = instr (Instruction $ Jump     simm)
jmpi_ct simm = instr (Instruction $ JumpCt simm)
jmpi_cf simm = instr (Instruction $ JumpCf simm)
-- wait    uimm = write 0b110011 (uimm10 uimm)

-- not rX = write 0b111000 (register rX)
abs rX = instr (Instruction $ Abs rX)
halt   = instr (Instruction $ Halt)
