{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
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
module ISA.Example.Add (addLowLevel, initCtx)  where

import           Control.Monad.State.Strict
import           Data.Int                           (Int32)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromJust)
import qualified Data.Set                           as Set
import qualified Data.Tree                          as Tree

import           ISA.Assembly
-- import           ISA.Backend.Dependencies
import           ISA.Backend.Symbolic.QueryList
-- import           ISA.Semantics
import           ISA.Backend.Symbolic.List.QueryRun
import           ISA.Example.Common
import           ISA.Types
import           ISA.Types.CTL
import           ISA.Types.CTL.Model
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Instruction.Encode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Parser
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
          , "Conditions: \n" <> unlines (map show (_constraints ctx))
  , showKey ctx IC
  , showKey ctx IR
  , showKey ctx (F Condition)
  , showKey ctx (F Halted)
  , showKey ctx (F Overflow)
  , showKey ctx (Reg R0)
  , showKey ctx (Addr 0)
  , showKey ctx (Addr 1)
  ] ++
  show (_solution ctx)
  ++ "\n==================="

initCtx :: Context
initCtx =
  MkContext { _pathCondition = true
            , _constraints = []
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
            , _solution = Nothing
            }

-- symexecTrace :: Int -> Trace Context
-- symexecTrace steps = runModel steps initCtx

-- noOverflow :: Script -> Symbolic SBool
-- noOverflow src = do
--     x <- forall "x"
--     y <- forall "y"
--     constrain $ x .>= 0 .&& x .<= 1000
--     constrain $ y .>= 0 .&& y .<= 1000
--     let prog = assemble src
--         steps = 100
--     let mem = mkMemory [(0, x), (1, y)]
--     let initialState = boot prog defaultRegisters mem defaultFlags
--         finalState = simulate steps initialState
--     let halted = readArray (flags finalState) (flagId Halt)
--         overflow = readArray (flags finalState) (flagId Overflow)
--     pure $ halted .&& sNot overflow

theorem :: Symbolic (Trace Context)
theorem = do
  undefined
  -- x <- forall "x"
  -- y <- forall "y"
  -- constrain ("x == 3", const (SEq x 3))
  -- constrain ("y == 5", const (SEq y 5))
  -- -- constrain ("x == y", const (SEq x y))
  -- -- constrain ("No overflow", \ctx -> SNot (fromJust (getBinding (F Overflow) ctx)))
  -- let mem = mkMemory [(0, x), (1, y)]
  -- initialState <- boot addLowLevel defaultRegisters mem defaultFlags

  -- tr <- liftIO $ runModel 10 initialState
  --     -- tr' = constrainTrace
  -- pure tr

demo_add :: IO ()
demo_add = do
  tr <- snd <$> runModel 10 initCtx



  -- mapM_ putStr$ fmap (\(Node n ctx) -> showContext ctx) (unTrace tr)
  let noOverflow =
        -- formulate (AllG . Atom $
        --            (\ctx -> maybe undefined id
        --                   . getBinding (F Overflow) $ ctx)) tra
        formulate (EG . Atom $
                   (\ctx -> maybe undefined id
                          . getBinding (F Overflow) $ ctx)) tr
  -- print (tryFoldConstant (subst 2 "y" (subst 3 "x" noOverflow)))
  mapM_ (putStrLn . show) =<< prove noOverflow
                  (ConstrainedBy $ [ (SAny "x" `SGt` 0) &&& (SAny "x" `SLt` 1000)
                                   , (SAny "y" `SGt` 0) &&& (SAny "y" `SLt` 1000)
                                   ]
                  )
  -- let prop1 = either undefined id $ parseProp "correct" "leafs allsat R0 \\v -> v == x + y"
  --     prop2 = either undefined id $
  --               parseProp "noOver" "whole allunsat Overflow \\v -> v"
  -- let correct = formulate prop1 tr
  --     noOverflow = formulate prop2 tr
  --       -- (InWhole (AllUnsat) (F Overflow) id) tr
  --     t = correct -- noOverflow
  -- case t of
  --   Left err  -> print err
  --   Right thm -> do
  --     print thm
  --     print =<< prove thm cs
  -- solved <- solveTrace (fst tr)
  -- let cs = fmap (\(Node _ s ctx) -> showContext ctx) (unTrace (fst tr))
  -- let z = fmap (\(Node _ ctx) -> showContext ctx) (unTrace . fst $ tr)
  -- mapM putStrLn z
  -- let tr' = fmap solveContext (fst tr)
  -- let z = Tree.foldTree (\(Node c s) xs -> s : concat xs) (unTrace tr')

  pure ()
