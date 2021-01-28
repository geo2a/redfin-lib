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

module ISA.Backend.Symbolic.List.QueryRun
                                           where

import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader.Class     ()
import           Control.Monad.State.Class
import           Control.Monad.State.Strict     (StateT, evalStateT, lift)
import           Data.Functor                   (void)
import           Data.IORef                     ()
import qualified Data.Map.Strict                as Map
import qualified Data.SBV.Dynamic               as SBV
import           Data.SBV.Internals             as SBV hiding (NodeId)
import qualified Data.SBV.Trans                 as SBV
import qualified Data.SBV.Trans.Control         as SBV
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           GHC.Stack
import           Prelude                        hiding (log)

import           ISA.Backend.Symbolic.QueryList
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT
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


pipeline :: (HasCallStack, MonadIO m)
         => Context -> m (InstructionCode, Context)
pipeline s =
    let steps = fetchInstruction *>
                incrementInstructionCounter *>
                readInstructionRegister
    in liftIO (runEngine steps s) >>=
       \case [result] -> pure result
             _ -> error $ "piplineStep: impossible happened:" <>
                  "fetchInstruction returned not a singleton."

-- | Perform one step of symbolic execution
step :: (HasCallStack, MonadIO m)
     => Context -> m [Context]
step s = do
    (ic, fetched) <- pipeline s
    let instrSemantics = case decode ic of
                           Nothing -> error $ "Engine.readInstructionRegister: " <>
                                              "unknown instruction with code " <> show ic
                           Just i -> instructionSemanticsM (symbolise i) readKey writeKey
    map snd <$> liftIO (runEngine instrSemantics fetched)

runModelM ::
  ( HasCallStack, MonadIO m
  , SBV.MonadQuery m, SBV.MonadSymbolic m, SolverContext m)
  => Map.Map Text SBV.SInt32 -> Int -> Context -> StateT NodeId m (Trace Context)
runModelM vars steps s = do
    n <- get
    modify (+ 1)
    let halted = case Map.lookup (F Halted) (_bindings s) of
          Just b  -> b
          Nothing -> error "Engine.runModel: uninitialised flag Halted!"
    if | steps <= 0 -> pure (mkTrace (Node n s) [])
       | otherwise  ->
         case halted of
           SConst (CBool True) -> pure (mkTrace (Node n s) [])
           _ -> do
             children <- liftIO $ step s
             solvedChildren <- lift $ mapM (processContext vars) children
             mkTrace (Node n s) <$> traverse (runModelM vars (steps - 1)) solvedChildren
  where fuel = 100

-- | Process a context, solving the constraints and putting the solution
--   into it
processContext ::
  ( HasCallStack, MonadIO m
  , SBV.MonadQuery m, SBV.MonadSymbolic m, SolverContext m)
  => Map.Map Text SBV.SInt32 -> Context -> m Context
processContext vars ctx = SBV.inNewAssertionStack $ do
  x <- toSMT vars [_pathCondition ctx]
  SBV.constrain x
  SBV.checkSat >>= \case
    SBV.Unk -> pure $ ctx { _solution = Nothing }
    _ -> SBV.getSMTResult >>= \case
      yes@(SBV.Satisfiable _ _) ->
        pure $ ctx { _solution = Just $ SBV.SatResult yes }
      no@(SBV.Unsatisfiable _ _) ->
        pure $ ctx { _solution = Just $ SBV.SatResult no }
      _ -> error "not implemented"

runModel :: Int -> Context -> IO (Trace Context)
runModel steps ctx = SBV.runSMT $ do
  let freeVars = gatherFree ((_pathCondition ctx) &&& conjoin (map snd $ _constraints ctx))
  vars <- createSym (Set.toList freeVars)
  SBV.query (evalStateT (runModelM vars steps ctx) 0)
