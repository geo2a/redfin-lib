module ISA.Example.Common where

import           Data.Bifunctor             (first)
import qualified Data.Map.Strict            as Map
import           Data.SBV

import           ISA.Assembly
import           ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context

defaultRegisters :: [(Register, Sym)]
defaultRegisters = [ (R0, 0)
                   , (R1, 1)
                   , (R2, 2)
                   , (R3, 3)
                   ]

defaultFlags :: [(Flag, Sym)]
defaultFlags = [ (Halted, SConst (CBool False))
               , (Overflow, SConst (CBool False))
               , (Condition, SConst (CBool False))
               , (Zero, SConst (CBool False))
               ]

mkMemory :: [(Address, Sym)] -> [(Address, Sym)]
mkMemory = id

boot :: Script
     -> [(Register, Sym)]
     -> [(Address,  Sym)]
     -> [(Flag, Sym)]
     -> Context
boot src regs memory flags =
  MkContext { _pathCondition = SConst (CBool True)
            , _bindings =
                Map.fromList $ mkProgram src
                            ++ map (first Reg) regs
                            ++ map (first Addr) memory
                            ++ map (first F) flags
                            ++ [(IC, SConst 0), (IR, 0)]
            }

prover :: SMTConfig
prover = z3 { verbose = True
            , redirectVerbose = Just "example.smt2"
            , timing = PrintTiming
            , printBase = 10
            }
