-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Symbolic.ZipperEngine.Run
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Symbolic simulation over a zipper-focused binary tree
-----------------------------------------------------------------------------

module ISA.Backend.Symbolic.ZipperEngine.Run (runModel) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class            (MonadIO (..))
import           Control.Monad.Reader
import           Control.Monad.Reader.Class        ()
import           Control.Monad.State.Class
import           Data.Functor                      (void)
import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.Map.Strict                   as Map
import qualified Data.SBV.Trans                    as SBV
import qualified Data.SBV.Trans.Control            as SBV
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Time.Clock                   (NominalDiffTime)
import           GHC.Stack
import           Prelude                           hiding (log)

import qualified ISA.Assembly                      as A
import           ISA.Backend.Symbolic.ZipperEngine
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Tree                    hiding (down, left, right,
                                                    top, up)

-- | Fetching an instruction is a Monadic operation. It is possible
--   (and natural) to implement in terms of @FS Key Monad Value@, but
--   for now we'll stick with this concrete implementation in the
--   @Engine@ monad.
fetchInstruction ::Engine (Data Sym)
fetchInstruction =
  readKey IC >>= \(MkData x) -> case (toAddress x) of
    Right ic ->
      writeKey IR (readKey (Prog ic))
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

-- | Perform one step of symbolic execution
step :: Engine (Maybe (OneTwo Context))
step = do
  void $ fetchInstruction
  incrementInstructionCounter
  ic <- readInstructionRegister
  -- decode and execute the instruction in the focused state
  case decode ic of
    Nothing -> error $ "Engine.readInstructionRegister: " <>
                       "unknown instruction with code " <> show ic
    Just i -> void $ instructionSemanticsM (symbolise i) readKey writeKey
  env <- ask
  -- observe if the instruction semantics triggered a fork
  (trace, choice) <- liftIO . atomically $ do
    r <- (,) <$> readTMVar (_trace env) <*> readTMVar (_choice env)
    -- clear the choice variable
    void $ swapTMVar (_choice env) Nothing
    pure r
  case choice of
    -- if no choice was encountered return a single child state
    Nothing           -> do
      focused <- getFocused
      (reachable, ctx) <- liftIO . SBV.runSMT $ reach focused
      if reachable then pure . Just $ One ctx
                   else pure Nothing
    -- if there is a choice: check reachability and return reachable children
    Just (ctx1, ctx2) -> do
      r1@(b1, ctx1') <- liftIO . SBV.runSMT $ reach ctx1
      r2@(b2, ctx2') <- liftIO . SBV.runSMT $ reach ctx2
      case (r1, r2) of
        -- both children are reachable
        ((True, ctx1'), (True, ctx2')) -> pure . Just $ Two ctx1' ctx2'
        ((False, _)   , (True, ctx2))  -> pure . Just $ One ctx2'
        ((True, ctx1) , (False, _))    -> pure . Just $ One ctx1'
        ((False, _)   , (False, _))    -> pure Nothing
  where
    -- | Check satisfiability of a Context's path condition under its constraints
    reach :: Context -> SBV.Symbolic (Bool, Context)
    reach ctx = do
      let freeVars = findFreeVars ctx
      vars <- createSym (Set.toList freeVars)
      constrs <- toSMT vars ((_pathCondition ctx):(map snd (_constraints ctx)))
      SBV.query $ do
        SBV.constrain constrs
        SBV.checkSat >>= \case
            SBV.Unk -> pure $ (False, ctx { _solution = Nothing })
            _ -> SBV.getSMTResult >>= \case
              (SBV.Satisfiable _ yes) -> do
                values <- traverse SBV.getValue vars
                pure $ (True, ctx { _solution = (Just . Satisfiable . MkSMTModel $ values) })
              (SBV.Unsatisfiable _ _) ->
                pure $ (False, ctx { _solution = Just $ Unsatisfiable })
              _ -> error "not implemented"

-- | Run symbolic simulation for a number of steps
runModel :: Int -> Context -> IO Trace
runModel steps init = execEngine (runModelImpl steps) init

runModelImpl :: Int -> Engine ()
runModelImpl steps = do
  env <- ask
  trace <- liftIO . atomically . readTMVar $ (_trace env)
  ctx <- getFocused
  case (||) <$> pure (steps <= 0)
            <*> (toBool <$> getBinding (F Halted) ctx) of
    Just True -> pure ()
    _ -> do
      -- perform a step originating in the state (n, ctx)
      choice <- step
      -- add one or two children at the focus point
      -- liftIO $ print $ (fmap (\c -> (_pathCondition c, _solution c))) <$> choice
      growTrace choice
      case choice of
        Nothing -> do
          -- no reachable children! we are done
          pure ()
        Just (One _) -> do
         -- go down the tree trunk and continue
         down
         runModelImpl (steps - 1)
        Just (Two _ _) -> do
          branch <- get
          -- go into the left subtree
          leftBottom <- left *> runModelImpl (steps - 1) *> get
          -- backtrack to the branch and go into the right subtree
          wayUp branch leftBottom >> right >> runModelImpl (steps -1)
  where
    wayUp :: Loc Int () -> Loc Int () -> Engine ()
    wayUp there = go
      where go here = do
             case locKey here == locKey there of
               True  -> put here
               False -> up >> get >>= go
