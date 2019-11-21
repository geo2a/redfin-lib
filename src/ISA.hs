-----------------------------------------------------------------------------
-- |
-- Module     : ISA
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
-----------------------------------------------------------------------------
module ISA
    (demo)
  where

import           Data.Int                      (Int32)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)

import           ISA.Assembly
import           ISA.Backend.Dependencies
import           ISA.Backend.Symbolic.List
-- import           ISA.Semantics
import           ISA.Backend.Symbolic.List.Run
import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Encode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Trace

mkProgram :: Script -> [(Key, Sym)]
mkProgram src =
  let prog = assemble src
      addrs = map Prog [0..]
      ics   = [ SConst (CWord ic) | (InstructionCode ic) <- map (encode . snd) prog]
  in zip addrs ics

src_ex1 :: Script
src_ex1 = do
  add R0 0
  add R1 1
  goto_ct "end"
  halt
  "end" @@ halt

demo :: IO ()
demo = do
  let ctx = MkContext { _pathCondition = SConst (CBool True)
                      , _bindings = Map.fromList $ [ (IC, SConst 0)
                                                   , (IR, 0)
                                                   , (F Condition, SConst (CBool False))
                                                   , (F Halted, SConst (CBool False))
                                                   , (Reg R0, SAny "r0")
                                                   , (Reg R1, SAny "r1")
                                                   , (Addr 0, SAny "a0")
                                                   , (Addr 1, SAny "a1")
                                                   ] ++ mkProgram src_ex1
                      , _fmapLog = []
                      }
  -- let program = [(0, Instruction $ Add R0 0)]
  --     dataGraph =
  --       fromJust $ programDataGraph (program :: [(Address, Instruction (Data Int32))])
  -- -- putStrLn "Data dependencies: "
  -- -- print (dependencies (instructionSemantics (snd . head $ program :: Instruction (Data Int32))))
  -- putStrLn ""
  -- putStrLn "Symbolic execution tree: "
  -- -- let t = instructionSemantics
  -- --       ((snd $ head program) :: Instruction (Data Sym)) readKey writeKey
  -- -- let xs = runEngine t ctx
  -- -- print $ xs
  -- let t = runModel 10 ctx
  -- putStrLn (renderTrace show t)
  debugConsole 10 ctx
  pure ()
