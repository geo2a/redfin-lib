-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Example.Sum
-- Copyright  : (c) Georgy Lukyanov 2019-2020
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental

-- Example program that finds the sum of numbers in an array

-----------------------------------------------------------------------------
module ISA.Example.Sum
    (demo_sum)
  where

import           Data.Int                      (Int32)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)

import           ISA.Assembly
-- import           ISA.Backend.Dependencies
import           ISA.Backend.Symbolic.List
-- import           ISA.Semantics
import           ISA.Backend.Symbolic.List.Run
import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Instruction.Encode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Trace

sumArrayLowLevel :: Script
sumArrayLowLevel = do
    let { pointer = 0; sum = 253; array_start = 255 } -- ; pointer_store
    let { r0 = R0; r1 = R1; r2 = R2 }
    -- ld_i r0 0
    -- st r0 sum
    ld r1 pointer
    -- add_i r1 1
    -- st r1 pointer

    -- compare the pointer variable to the array_start
    "loop" @@ cmplt r1 array_start
    -- if pointer == array_start then terminate
    goto_ct "end"
    -- jmpi_ct 7

    ldmi r2 pointer
    add r2 sum
    st r2 sum
    -- ld r1 pointer
    sub_i r1 1
    st r1 pointer

    goto "loop"
    "end" @@ ld r0 sum
    halt

showContext :: Context -> String
showContext ctx =
  unlines [ "Path constraint: " <> show (_pathCondition ctx)
  , showKey ctx IC
  , showKey ctx IR
  , showKey ctx (F Condition)
  , showKey ctx (F Halted)
  , showKey ctx (Reg R0)
  , showKey ctx (Reg R1)
  , showKey ctx (Reg R2)
  , showKey ctx (Addr 0)
  , showKey ctx (Addr 253)
  , showKey ctx (Addr 255)
  ]

demo_sum :: IO ()
demo_sum = do
  let ctx = MkContext { _pathCondition = SConst (CBool True)
                      , _bindings = Map.fromList $ [ (IC, SConst 0)
                                                   , (IR, 0)
                                                   , (F Condition, SConst (CBool False))
                                                   , (F Halted, SConst (CBool False))
                                                   , (Reg R0, 0)
                                                   , (Reg R1, 0)
                                                   , (Reg R2, 0)
                                                   , (Addr 0, 3)
                                                   , (Addr 253, 0)
                                                   , (Addr 255, 1)

                                                   , (Addr 1, SAny "x1")
                                                   , (Addr 2, SAny "x2")
                                                   , (Addr 3, SAny "x3")
                                                   ] ++ mkProgram sumArrayLowLevel
                      , _fmapLog = []
                      }
  -- let program = [(0, Instruction $ Add R0 0)]
  --     dataGraph =
  --       fromJust $ programDataGraph (program :: [(Address, Instruction (Data Int32))])
  -- -- putStrLn "Data dependencies: "
  -- -- print (dependencies (instructionSemantics (snd . head $ program :: Instruction (Data Int32))))
  -- putStrLn ""
  putStrLn "Symbolic execution tree: "

  let t = runModel 50 ctx
      tracePath = "/home/geo2a/Desktop/traces/trace_sum.html"
  writeTraceHtmlFile showContext tracePath t
  putStrLn $ "Wrote trace into file " <> tracePath

  -- debugConsole 10 ctx
  pure ()
