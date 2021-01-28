{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
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
  ( demo_sum
  , sumArrayLowLevel
  , initCtx
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Data.Int                           (Int32)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromJust)

import           ISA.Assembly
-- import           ISA.Backend.Dependencies
import           ISA.Backend.Symbolic.QueryList
import           ISA.Types.Symbolic.Context
-- import           ISA.Semantics
import           ISA.Backend.Symbolic.List.QueryRun
import           ISA.Example.Common
import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Instruction.Encode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.SMT
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
  , showKey ctx (F Overflow)
  , showKey ctx (Reg R0)
  , showKey ctx (Reg R1)
  , showKey ctx (Reg R2)
  , showKey ctx (Addr 0)
  , showKey ctx (Addr 253)
  , showKey ctx (Addr 255)
  ]

initCtx :: Context
initCtx = MkContext
  { _pathCondition = SConst (CBool True)
  , _constraints =
    [ ("0 < x1 < 100", ((SGt (SAny "x1") 0) &&& (SLt (SAny "x1") 100)))
    , ("0 < x2 < 100", ((SGt (SAny "x2") 0) &&& (SLt (SAny "x2") 100)))
    , ("0 < x3 < 100", ((SGt (SAny "x3") 0) &&& (SLt (SAny "x3") 100)))
    ]
  , _bindings = Map.fromList $ [ (IC, SConst 0)
                               , (IR, 0)
                               , (F Condition, SConst (CBool False))
                               , (F Halted, SConst (CBool False))
                               , (F Overflow, SConst (CBool False))
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
  , _solution = Nothing
  }


theorem :: Symbolic (Trace Context)
theorem = do
  undefined
  -- x1 <- forall "x1"
  -- x2 <- forall "x2"
  -- x3 <- forall "x3"

  -- constrain ("0 < x1 < 100", const $ (SGt x1 0) &&& (SLt x1 100))
  -- constrain ("0 < x2 < 100", const $ (SGt x2 0) &&& (SLt x2 100))
  -- constrain ("0 < x3 < 100", const $ (SGt x3 0) &&& (SLt x3 100))

  -- let mem = mkMemory [(0, 3), (1, x1), (2, x2), (3, x3), (253, 0), (255, 1)]
  -- initialState <- boot sumArrayLowLevel defaultRegisters mem defaultFlags

  -- liftIO (runModel 100 initialState)

demo_sum :: IO ()
demo_sum = do
  -- -- let ctx = MkContext { _pathCondition = SConst (CBool True)
  -- let ctx = MkContext { _pathCondition = ((SGt (SAny "x1") 0) &&& (SLt (SAny "x1") 100))
  --                                    &&& ((SGt (SAny "x2") 0) &&& (SLt (SAny "x2") 100))
  --                                    &&& ((SGt (SAny "x3") 0) &&& (SLt (SAny "x3") 100))
  --                     , _bindings = Map.fromList $ [ (IC, SConst 0)
  --                                                  , (IR, 0)
  --                                                  , (F Condition, SConst (CBool False))
  --                                                  , (F Halted, SConst (CBool False))
  --                                                  , (F Overflow, SConst (CBool False))
  --                                                  , (Reg R0, 0)
  --                                                  , (Reg R1, 0)
  --                                                  , (Reg R2, 0)
  --                                                  , (Addr 0, 3)
  --                                                  , (Addr 253, 0)
  --                                                  , (Addr 255, 1)

  --                                                  , (Addr 1, SAny "x1")
  --                                                  , (Addr 2, SAny "x2")
  --                                                  , (Addr 3, SAny "x3")
  --                                                  ] ++ mkProgram sumArrayLowLevel
  --                     }
  tr <- runSymbolic theorem
  -- solved <- solveTrace (fst tr)
  -- let cs = fmap (\(Node _ s ctx) -> showContext ctx) (unTrace (fst tr))
  let z = fmap (\(Node _ ctx) -> showContext ctx) (unTrace . fst $ tr)
  mapM putStrLn z


  -- let program = [(0, Instruction $ Add R0 0)]
  --     dataGraph =
  --       fromJust $ programDataGraph (program :: [(Address, Instruction (Data Int32))])
  -- -- putStrLn "Data dependencies: "
  -- -- print (dependencies (instructionSemantics (snd . head $ program :: Instruction (Data Int32))))
  -- putStrLn ""
  -- putStrLn "Symbolic execution tree: "

  -- let t = runModel 1000 ctx
  --     tracePath = "/home/geo2a/Desktop/traces/trace_sum.html"
  -- writeTraceHtmlFile showContext tracePath t
  -- putStrLn $ "Wrote trace into file " <> tracePath

  -- let ps = map (map nodeBody) $ paths (unTrace t)
  --     leaves = map (last . map nodeBody) $ paths (unTrace t)
  --     ps' = map solvePath ps
  --     overflownPaths = filter id $ map (any isSat) ps'
  -- print $ ps'
  -- print overflownPaths
  -- Ok, it's kinda working, but not really: job for tomorrow is
  -- to find out a good way to establish preconditions
  -- if all id ps'' then
  --   putStrLn "Yes"
  --   else putStrLn "No"
  -- print $ map solveContext leaves

  -- debugConsole 10 ctx
  pure ()
