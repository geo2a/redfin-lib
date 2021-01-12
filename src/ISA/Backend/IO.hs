{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.IO
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Simplistic IO-based interpreter for ISA

-----------------------------------------------------------------------------

module ISA.Backend.IO
    ( bootRuntimeIO
    , simulateIO
    ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader
import           Control.Selective
import           Data.Int                   (Int32)
import           Data.IORef
import qualified Data.Map                   as Map
import           FS
import           Prelude                    hiding (Read, init, readIO)

import           ISA.Types

-- | Simulator environment
data Env a = MkEnv
  { bindings :: IORef (Map.Map Key a)
  }

-- | Initialise state of the runtime
bootRuntimeIO :: [(Key, a)] -> IO (Env a)
bootRuntimeIO init =
  MkEnv <$> newIORef (Map.fromList init)

readIO :: Key -> ReaderT (Env a) IO a
readIO key = do
  boundKeys <- liftIO . readIORef . bindings =<< ask
  case Map.lookup key boundKeys of
       Nothing  -> error $ "Undefined key: " <> show key
       Just val -> do
         liftIO $ putStrLn $ "Read: " <> show key
         pure val

writeIO :: Show a => Key -> ReaderT (Env a) IO a -> ReaderT (Env a) IO a
writeIO key producer = do
  env <- ask
  value <- liftIO (runReaderT producer env)
  env' <- ask
  boundKeys <- liftIO . readIORef . bindings $ env'
  liftIO $ writeIORef (bindings env) (Map.adjust (const value) key boundKeys)
  liftIO $ putStrLn $ "Write: (" <> show key <> ", " <> show value <> ")"
  pure value

simulateIO :: Env (Data Int32)
           -> FS Key Selective Value (Data Int32)
           -> IO (Data Int32)
simulateIO env comp = do
  runReaderT (comp readIO writeIO) env
