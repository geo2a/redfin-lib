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
import           Data.Bifunctor
import           Data.Int                              (Int32)
import qualified Data.IntMap                           as IntMap
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromJust)
import qualified Data.Set                              as Set

import           Algebra.Graph.Export.Dot

import           ISA.Assembly
import           ISA.Backend.Symbolic.TransitionSystem
-- import           ISA.Backend.Dependencies
-- import           ISA.Semantics
import           ISA.Backend.Symbolic.Zipper
import           ISA.Backend.Symbolic.Zipper.Run
import           ISA.Example.Common
import           ISA.Types
import           ISA.Types.CTL
import           ISA.Types.CTL.Model
import           ISA.Types.Context                     hiding (Context)
import qualified ISA.Types.Context                     as ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Instruction.Encode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Parser
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Tree

type Context = ISA.Types.Context Sym


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
                                         , (Reg R0, 0)
                                         , (Addr 0, SAny "x")
                                         , (Addr 1, SAny "y")
                                         ] ++ mkProgram addLowLevel
            , _solution = Nothing
            }
demo_add :: IO ()
demo_add = do
  tr <- runModel 10 initCtx
  mapM_ putStrLn (draw (_layout tr))
  mapM_ (putStrLn . show) (_states tr)
  -- mapM_ putStr$ fmap (\(Node n ctx) -> showContext ctx) (unTrace tr)
  -- let noOverflow = (AllG . Not . Atom $
  --                  (\ctx -> maybe undefined id
  --                         . getBinding (F Overflow) $ ctx))
  --     correct = (AllF . Atom $
  --               (\ctx -> SEq (SAdd (SAny "x") (SAny "y")) . maybe undefined id
  --                      . getBinding (Reg R0) $ ctx))
  --     halted = (AllF . Atom $
  --               (\ctx -> maybe undefined id
  --                      . getBinding (F Halted) $ ctx)
  -- let phi = And noOverflow (And halted correct)
  -- putStrLn $ "Property: " <> show (formulate (Not phi) tr)
  -- putStrLn $ "Property': " <> show (formulate psi tr)
  -- ans <- prove phi tr $
  --        ConstrainedBy $ [ (SAny "x" `SGt` 0) &&& (SAny "x" `SLt` 1000)
  --                        -- , (SAny "y" `SGt` 0) &&& (SAny "y" `SLt` 1000)
  --                        ]
  -- print ans
  pure ()

  pure ()
