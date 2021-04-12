module ISA.Example.Common where

import Data.SBV (SMTConfig (..), Timing (..), z3)

import ISA.Types
import ISA.Types.Symbolic
import ISA.Types.Symbolic.Address

defaultRegisters :: [(Register, Sym)]
defaultRegisters =
    [ (R0, 0)
    , (R1, 1)
    , (R2, 2)
    , (R3, 3)
    ]

defaultFlags :: [(Flag, Sym)]
defaultFlags =
    [ (Halted, SConst (CBool False))
    , (Overflow, SConst (CBool False))
    , (Condition, SConst (CBool False))
    ]

mkMemory :: [(Address, Sym)] -> [(Address, Sym)]
mkMemory = id

prover :: SMTConfig
prover =
    z3
        { verbose = True
        , redirectVerbose = Just "example.smt2"
        , timing = PrintTiming
        , printBase = 10
        }
