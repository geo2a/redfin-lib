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

module ISA.Backend.Symbolic.List.Run
    (debugConsole, runModel) where

import           Colog.Core                   (LogAction (..), logStringStdout,
                                               (<&))
import           Control.Monad                (ap, join, liftM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader.Class
import           Control.Monad.State          (evalState)
import           Control.Monad.State.Class
import           Data.Functor                 (void)
import           Data.IORef
import qualified Data.Map.Strict              as Map
import           Prelude                      hiding (log)
import           System.Console.Haskeline

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
incrementInstructionCounter =
  void $ writeKey IC ((+ 1) <$> readKey IC)

readInstructionRegister :: Engine InstructionCode
readInstructionRegister =  do
  x <- (fmap toInstructionCode) <$> readKey IR
  case x of
    (MkData (Right ic)) -> pure ic
    (MkData (Left sym)) -> error $ "Engine.readInstructionRegister: " <>
                           "symbolic instruction code encountered " <> show sym


pipeline :: Context -> (InstructionCode, Context)
pipeline s =
    let steps = do fetchInstruction
                   incrementInstructionCounter
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
                           Just i -> instructionSemantics (symbolise i) readKey writeKey
    in map snd $ runEngine instrSemantics fetched

runModelM :: MonadState NodeId m =>
   Int -> Context -> m (Trace Context)
runModelM steps s = do
    modify (+ 1)
    n <- get
    let h = case Map.lookup (F Halted) (_bindings s) of
          Just b  -> b
          Nothing -> error "Engine.runModel: uninitialised flag Halted!"
    let halted    = h == SConst (CBool True)
        newStates = step s
    if | steps <= 0 -> pure (mkTrace (Node n s) [])
       | otherwise  ->
         if halted then pure (mkTrace (Node n s) [])
                   else do children <- traverse (runModelM (steps - 1)) newStates
                           pure $ mkTrace (Node n s) children

runModel :: Int -> Context -> Trace Context
runModel steps s = evalState (runModelM steps s) 0

data DebugEnv = MkDebugEnv
  { _log    :: LogAction IO String
  , _ui     :: Context -> IO ()
  , _stepId :: IORef NodeId
  -- , _stepsLeft  :: IORef Int
  }

newtype Debugger a = MkDebugger {runDebugger :: DebugEnv -> IO a}
  deriving Functor

instance Applicative Debugger where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Monad Debugger where
  return x = MkDebugger $ \_ -> return x
  x >>= f = MkDebugger $ \env -> do
    y <- runDebugger x env
    runDebugger (f y) env

instance (MonadReader DebugEnv) Debugger where
  ask = MkDebugger $ \env -> pure env
  local = error "Debugger.MonadReader.local is not implemented"

instance (MonadState NodeId) Debugger where
  get = MkDebugger $ \env ->
    readIORef (_stepId env)
  put s = MkDebugger $ \env ->
    writeIORef (_stepId env) s
  -- modify f = MkDebugger $ \env ->
  --   modifyIoRef (_stepId env) s

instance MonadIO Debugger where
  liftIO f = MkDebugger $ \env -> f

instance MonadException Debugger where
  controlIO f = MkDebugger $ \env -> controlIO $ \(RunIO run) -> let
                  run' = RunIO (fmap (MkDebugger . const) . run . flip runDebugger env)
                  in fmap (flip runDebugger env) $ f run'

debugImpl :: Int -> Context -> InputT Debugger (Trace Context)
debugImpl steps ctx = do
  -- log <- _log <$> ask
  -- ui  <- _ui <$> ask
  -- liftIO $ ui ctx
  -- modify (+ 1)
  -- n <- get
  let n = -1
  outputStrLn "Current state: "
  outputStrLn $ show ctx
  minput <- getInputLine "% "
  case minput of
      Nothing -> pure (mkTrace (Node n ctx) [])
      Just "quit" -> pure (mkTrace (Node n ctx) [])
      Just input -> do
        outputStrLn $ "Input was: " ++ input
        let h = case Map.lookup (F Halted) (_bindings ctx) of
              Just b  -> b
              Nothing -> error "Engine.runModel: uninitialised flag Halted!"
        let halted    = h == SConst (CBool True)
            newStates = step ctx
        if | steps <= 0 -> pure (mkTrace (Node n ctx) [])
           | otherwise  ->
             if halted then pure (mkTrace (Node n ctx) [])
                       else do children <- traverse (debugImpl (steps - 1)) newStates
                               pure $ mkTrace (Node n ctx) children

debugConsole :: Int -> Context -> IO ()
debugConsole steps ctx = do
  nodeRef <- newIORef 0
  let env = MkDebugEnv undefined undefined nodeRef
  runDebugger (runInputT defaultSettings (debugImpl steps ctx)) undefined
  putStrLn "Debug session ended."
