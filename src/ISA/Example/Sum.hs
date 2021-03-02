{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
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

import           Control.Monad.IO.Class          (liftIO)
import           Data.Int                        (Int32)
import qualified Data.IntMap                     as IntMap
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust)

import           ISA.Assembly
-- import           ISA.Backend.Dependencies
import           ISA.Types.Context               hiding (Context)
import qualified ISA.Types.Context               as ISA.Types
-- import           ISA.Semantics
-- import           ISA.Backend.Graph
-- import           ISA.Backend.Graph.BasicBlock
import           ISA.Backend.Symbolic.Zipper
import           ISA.Backend.Symbolic.Zipper.Run
import           ISA.Example.Common
import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Instruction.Encode
import           ISA.Types.Key
import           ISA.Types.Prop
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address
import           ISA.Types.Symbolic.Logic.Adhoc
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Tree

-- type Context = ISA.Types.Context (Data Sym)

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
  { _pathCondition = MkData (SConst (CBool True))
  , _store = Map.empty
  , _constraints =
    [ ("0 < x1 < 100", MkData $ ((SGt (SAny "x1") 0) &&& (SLt (SAny "x1") 100)))
    , ("0 < x2 < 100", MkData $ ((SGt (SAny "x2") 0) &&& (SLt (SAny "x2") 100)))
    , ("0 < x3 < 100", MkData $ ((SGt (SAny "x3") 0) &&& (SLt (SAny "x3") 100)))
    , ("0 < n < 4", MkData $ ((SGt (SAny "n") 0) &&& (SLt (SAny "n") 10)))
--    , ("n == 3", MkData $ ((SEq (SAny "n") 3)))
    ]
  , _bindings = Map.fromList $ [ (IC, 0)
--                               , (IR, )
                               , (Reg R0, 0)
                               , (Reg R1, 0)
                               , (Reg R2, 0)
                             , (Addr 0, MkData $ SAny "n")
                               -- , (Addr 0, 3)
                               , (Addr 253, 0)
                               , (Addr 255, 1)
                               , (Addr 1, MkData $ SAny "x1")
                               , (Addr 2, MkData $ SAny "x2")
                               , (Addr 3, MkData $ SAny "x3")
                               , (F Halted, false)
                               , (F Condition, false)
                               , (F Overflow, false)
                               ] ++ mkProgram sumArrayLowLevel
  , _solution = Nothing
  }


demo_sum :: IO ()
demo_sum = do
  trace <- runModel 50 initCtx
  mapM_ putStrLn (draw (_layout trace))
  -- mapM_ (\s -> putStrLn . show $ (getBinding (Reg R0) s)) (_states trace)
  let correct = InLeafs $ \s ->
        SNot (
           SAnd (SEq (_unData (fromJust (getBinding (Reg R0) s)))
                      (SAny "x1" + SAny "x2" + SAny "x3"))
                (SAnd (SEq (_unData (fromJust (getBinding (Reg R1) s)))
                           (_unData (fromJust (getBinding (Addr 255) s))))
                      ((_unData (fromJust (getBinding (F Halted) s)))
                       ))
           )
  -- let trivial = InLeafs $ const true
  r <- sat correct trace (ConstrainedBy (map _unData . map snd $ _constraints initCtx))
  print r
  -- -- case r of
  -- --   Conjunct [Literal (n, Satisfiable s)] -> print ( modelAssocs s)
  print (getBinding (F Halted) $ fromJust $ IntMap.lookup 23 (_states trace))
  print (getBinding (F Halted) $ fromJust $ IntMap.lookup 32 (_states trace))
  pure ()
