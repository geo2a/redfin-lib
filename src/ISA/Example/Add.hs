-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Example.Add
-- Copyright  : (c) Georgy Lukyanov 2019-2020
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental

-- A really basic example of a program that adds two numbers to
-- demonstrate integer overflow detection and functional verification

-----------------------------------------------------------------------------
module ISA.Example.Add (
  demo_add
  ) where

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
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Symbolic.Trace

addLowLevel :: Script
addLowLevel = do
    let { x = 0; y = 1 } -- ; pointer_store
    let { r0 = R0 }
    ld r0 x
    add r0 y
    halt

showContext :: Context -> String
showContext ctx =
  unlines [ "Path constraint: " <> show (_pathCondition ctx)
  , showKey ctx IC
  , showKey ctx IR
  , showKey ctx (F Condition)
  , showKey ctx (F Halted)
  , showKey ctx (F Overflow)
  , showKey ctx (Reg R0)
  , showKey ctx (Addr 0)
  , showKey ctx (Addr 1)
  ]

demo_add :: IO ()
demo_add = do
  -- let ctx = MkContext { _pathCondition = SConst (CBool True)
  let ctx = MkContext { _pathCondition = true
                        -- ((SGt (SAny "x") 0) &&& (SLt (SAny "x") 100))
                        --              &&& ((SGt (SAny "y") 0) &&& (SLt (SAny "y") 100))
                      , _bindings = Map.fromList $ [ (IC, SConst 0)
                                                   , (IR, 0)
                                                   , (F Condition, SConst (CBool False))
                                                   , (F Halted, SConst (CBool False))
                                                   , (F Overflow, SConst (CBool False))
                                                   , (Reg R0, 0)
                                                   , (Addr 0, SAny "x")
                                                   , (Addr 1, SAny "y")
                                                   -- , (Addr 0, maxBound)
                                                   -- , (Addr 1, 1)
                                                   ] ++ mkProgram addLowLevel
                      }
  -- let program = [(0, Instruction $ Add R0 0)]
  --     dataGraph =
  --       fromJust $ programDataGraph (program :: [(Address, Instruction (Data Int32))])
  -- -- putStrLn "Data dependencies: "
  -- -- print (dependencies (instructionSemantics (snd . head $ program :: Instruction (Data Int32))))
  -- putStrLn ""
  putStrLn "Symbolic execution tree: "

  let t = runModel 1000 ctx
      tracePath = "/home/geo2a/Desktop/traces/trace_add.html"
  writeTraceHtmlFile showContext tracePath t
  putStrLn $ "Wrote trace into file " <> tracePath

  let ps = map (map nodeBody) $ paths (unTrace t)
  --     leaves = map (last . map nodeBody) $ paths (unTrace t)
      ps' = map solvePath ps
      overflownPaths = filter id $ map (all isSat) ps'
  print $ ps'
  -- print overflownPaths
  -- Ok, it's kinda working, but not really: job for tomorrow is
  -- to find out a good way to establish preconditions
  -- if all id ps'' then
  --   putStrLn "Yes"
  --   else putStrLn "No"
  -- print $ map solveContext leaves

  -- debugConsole 10 ctx
  pure ()
