{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- |
 Module     : ISA.Assembly
 Copyright  : (c) Georgy Lukyanov 2019
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Shallowly-embedded assembly language.
-}
module ISA.Assembly where

import Control.Monad.State
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import ISA.Types
import ISA.Types.Instruction
import ISA.Types.Instruction.Encode
import ISA.Types.Key
import ISA.Types.Symbolic
import ISA.Types.Symbolic.Address

decIfNeg :: Integral a => a -> a
decIfNeg x
    | x < 0 = x - 1
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
            let offset =
                    (fromIntegral there :: Int32)
                        - (fromIntegral here :: Int32)
                        - 1
            jmpi (fromIntegral offset)

-- | Go to a label
goto_ct :: Label -> Script
goto_ct name = do
    s <- get
    here <- instructionCounter <$> get
    case Map.lookup name (labels s) of
        Nothing -> jmpi 0
        Just there -> do
            let offset =
                    (fromIntegral there :: Int32)
                        - (fromIntegral here :: Int32)
                        - 1
            jmpi_ct (fromIntegral offset)

goto_cf :: Label -> Script
goto_cf name = do
    s <- get
    here <- instructionCounter <$> get
    case Map.lookup name (labels s) of
        Nothing -> jmpi 0
        Just there -> do
            let offset =
                    (fromIntegral there :: Int32)
                        - (fromIntegral here :: Int32)
                        - 1
            jmpi_cf (fromIntegral offset)

type Labels = Map.Map Label CAddress

data AssemblerState = MkAssemblerState
    { program :: [(CAddress, Instruction Int32)]
    , labels :: Labels
    , instructionCounter :: CAddress
    }

type Script = State AssemblerState ()

collectLabels :: Script -> Labels
collectLabels src =
    labels $ snd $ runState src (MkAssemblerState [] Map.empty 0)

assemble :: Script -> [(CAddress, Instruction Int32)]
assemble src =
    prg
  where
    prg = reverse $ program $ snd $ runState src (MkAssemblerState [] labels 0)
    labels = collectLabels src

mkProgram :: Script -> [(Key, Sym)]
mkProgram src =
    let prog = assemble src
        addrs = map Prog (iterate inc 0)
        ics = [SConst (CWord ic) | (InstructionCode ic) <- map (encode . snd) prog]
     in zip addrs ics

mkProgram1 :: Script -> [(Key, Int32)]
mkProgram1 src =
    let prog = assemble src
        addrs = map Prog (iterate inc 0)
        ics = [fromIntegral ic | (InstructionCode ic) <- map (encode . snd) prog]
     in zip addrs ics

instr :: Instruction Int32 -> Script
instr i = do
    s <- get
    let ic = instructionCounter s
    put $ s{program = (ic, i) : program s, instructionCounter = ic + 1}

(@@) :: Label -> Script -> Script
name @@ src = do
    label name
    src

label :: Label -> Script
label name = do
    s <- get
    let ic = instructionCounter s
    put $ s{labels = Map.insert name ic $ labels s}

-- Instruction mnemonics
add rX dmemaddr = instr (Instruction $ Add @Value rX dmemaddr)
add_i rX imm = instr (Instruction $ AddI @Value rX imm)
sub rX dmemaddr = instr (Instruction $ Sub @Value rX dmemaddr)
sub_i rX imm = instr (Instruction $ SubI @Value rX imm)
mul rX dmemaddr = instr (Instruction $ Mul @Value rX dmemaddr)
div rX dmemaddr = instr (Instruction $ Div @Value rX dmemaddr)
mod rX dmemaddr = instr (Instruction $ Mod @Value rX dmemaddr)
ld rX dmemaddr = instr (Instruction $ Load @Value rX dmemaddr)
st rX dmemaddr = instr (Instruction $ Store @Value rX dmemaddr)
ldmi rX dmemaddr = instr (Instruction $ LoadMI @Value rX dmemaddr)

-- stmi  rX dmemaddr = instr (Instruction StoreMI $ rX @Value dmemaddr)
cmpeq rX dmemaddr = instr (Instruction $ CmpEq @Value rX dmemaddr)
cmplt rX dmemaddr = instr (Instruction $ CmpLt @Value rX dmemaddr)
cmpgt rX dmemaddr = instr (Instruction $ CmpGt @Value rX dmemaddr)

-- sl    rX dmemaddr = instr (Instruction $ rX @Value dmemaddr)
-- sr    rX dmemaddr = instr (Instruction $ rX @Value dmemaddr)
-- sra   rX dmemaddr = instr (Instruction $ rX @Value dmemaddr)

-- add_si rX simm = write 0b100000 (register rX .|. simm8 simm)
-- sub_si rX simm = write 0b100001 (register rX .|. simm8 simm)
-- mul_si rX simm = write 0b100010 (register rX .|. simm8 simm)
-- div_si rX simm = write 0b100011 (register rX .|. simm8 simm)
ld_i rX simm = instr (Instruction $ Set @Value rX simm)

-- sl_i   rX uimm = write 0b101100 (register rX .|. uimm8 uimm)
-- sr_i   rX uimm = write 0b101101 (register rX .|. uimm8 uimm)
-- sra_i  rX uimm = write 0b101110 (register rX .|. uimm8 uimm)
-- ld_i   rX uimm = write 0b101111 (register rX .|. uimm8 uimm)

jmpi simm = instr (Instruction $ Jump @Value simm)
jmpi_ct simm = instr (Instruction $ JumpCt @Value simm)
jmpi_cf simm = instr (Instruction $ JumpCf @Value simm)

-- wait    uimm = write 0b110011 (uimm10 uimm)

-- not rX = write 0b111000 (register rX)
abs rX = instr (Instruction $ Abs @Value rX)
halt = instr (Instruction $ Halt @Value)

--------------------------------------------------------------------------------
