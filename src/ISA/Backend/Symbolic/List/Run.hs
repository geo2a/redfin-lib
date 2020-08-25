{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Symbolic.List.Run
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Run the symbolic execution ith multiple worlds semantics
--
-----------------------------------------------------------------------------

module ISA.Backend.Symbolic.List.Run ( runModel
                                     ) where

import           Control.Monad.IO.Class       ()
import           Control.Monad.Reader.Class   ()
import           Control.Monad.State          (evalState)
import           Control.Monad.State.Class
import           Data.Functor                 (void)
import           Data.IORef                   ()
import qualified Data.Map.Strict              as Map
import           Prelude                      hiding (log)

import           ISA.Backend.Symbolic.List
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Trace

-- | Fetching an instruction is a Monadic operation. It is possible
--   (and natural) to implement in terms of @FS Key Monad Value@, but
--   for now we'll stick with this concrete implementation in the
--   @Engine@ monad.
fetchInstruction :: Engine ()
fetchInstruction =
  readKey IC >>= \(MkData x) -> case (toAddress x) of
    Right ic -> void $ writeKey IR (readKey (Prog ic))
    Left sym ->
      error $ "Engine.fetchInstruction: symbolic or malformed instruction counter "
            <> show sym

incrementInstructionCounter :: Engine ()
incrementInstructionCounter = do
  void $ writeKey IC ((simplify Nothing <$>) <$> (+ 1) <$> readKey IC)

readInstructionRegister :: Engine InstructionCode
readInstructionRegister =  do
  x <- (fmap toInstructionCode) <$> readKey IR
  case x of
    (MkData (Right instr)) -> pure instr
    (MkData (Left sym)) -> error $ "Engine.readInstructionRegister: " <>
                           "symbolic instruction code encountered " <> show sym


pipeline :: Context -> (InstructionCode, Context)
pipeline s =
    let steps = fetchInstruction >>
                   incrementInstructionCounter >>
                   readInstructionRegister
    in case runEngine steps s of
            [result] -> result
            _ -> error $ "piplineStep: impossible happened:" <>
                         "fetchInstruction returned not a singleton."

-- | Perform one step of symbolic execution
step :: Context -> [Context]
step s =
    let (ic, fetched) = pipeline s
        instrSemantics = case decode ic of
                           Nothing -> error $ "Engine.readInstructionRegister: " <>
                                              "unknown instruction with code " <> show ic
                           Just i -> instructionSemanticsM (symbolise i) readKey writeKey
    in map snd $ runEngine instrSemantics fetched

runModelM :: MonadState NodeId m =>
   Int -> Context -> m (Trace Context)
runModelM steps s = do
    n <- get
    modify (+ 1)
    let halted = case Map.lookup (F Halted) (_bindings s) of
          Just b  -> b
          Nothing -> error "Engine.runModel: uninitialised flag Halted!"
    let newStates = step s
    if | steps <= 0 -> pure (mkTrace (Node n s) [])
       | otherwise  ->
         case halted of
           SConst (CBool True) -> pure (mkTrace (Node n s) [])
           _ -> do
             children <- traverse (runModelM (steps - 1)) newStates
             pure $ mkTrace (Node n s) children

runModel :: Int -> Context -> Trace Context
runModel steps s = evalState (runModelM steps s) 0
