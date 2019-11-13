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

import           Data.Int                     (Int32)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)

import           ISA.Backend.Dependencies
import           ISA.Backend.Symbolic.List
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Encode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Trace

mkProgram :: [Instruction (Data Int32)] -> [(Key, Sym)]
mkProgram prog =
  let addrs = map Prog [0..]
      ics   = [ SConst (CWord ic) | (InstructionCode ic) <- map encode prog]
  in zip addrs ics

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
                                                   ] ++
                                                   mkProgram [ mkI $ Add R0 0
                                                             , mkI $ JumpCt 1
                                                             , mkI $ Halt
                                                             , mkI $ Halt
                                                             ]
                      , _fmapLog = []
                      }
  -- let program = [(0, Instruction $ Add R0 0)]
  --     dataGraph =
  --       fromJust $ programDataGraph (program :: [(Address, Instruction (Data Int32))])
  -- -- putStrLn "Data dependencies: "
  -- -- print (dependencies (instructionSemantics (snd . head $ program :: Instruction (Data Int32))))
  -- putStrLn ""
  putStrLn "Symbolic execution tree: "
  -- let t = instructionSemantics
  --       ((snd $ head program) :: Instruction (Data Sym)) readKey writeKey
  -- let xs = runEngine t ctx
  -- print $ xs
  let t = runModel 10 ctx
  putStrLn (renderTrace show t)
  pure ()
