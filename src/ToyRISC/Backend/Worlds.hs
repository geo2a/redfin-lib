{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Backend.Symbolic
-- Copyright  : (c) Georgy Lucknow 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- A simple symbolic execution back-end
-- inspired by [SBV](http://hackage.haskell.org/package/sbv)
-- but more fine-tuned for executing low-level code
--
-----------------------------------------------------------------------------
module ToyRISC.Backend.Worlds
    -- ( Env(..), Engine (..), run
    -- , State (..), bindings
    -- , pathCond, model -- lenses for State
    -- , ) where
  where

-- import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Selective
import           Data.IORef
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust)
import           Data.Word            (Word8)
import           Lens.Micro.Platform
import           Prelude              hiding (log, read, readIO)

import           ToyRISC.SMT
import           ToyRISC.Symbolic
import           ToyRISC.Types
import           Tree                 (Tree)
import qualified Tree                 as Tree


-- -- | State of the symbolic interpreter
-- data State modelTy = State
--               { _pathCond :: SExpr
--               -- ^ path condition: a list of symbolic expressions of kind Bool
--               , _model    :: modelTy
--               -- ^ the state of the runtime being modelled, i.e. processor or
--               --   virtual machine
--               } deriving (Show, Eq)

-- makeLenses ''State

data Context = MkContext { _bindings      :: Map.Map Key SExpr
                         , _pathCondition :: SExpr
                         }

makeLenses ''Context

instance Show Context where
  show ctx = unlines [ show (_pathCondition ctx)
                     , show (Map.toList $ _bindings ctx)
                     ]

type WorldId = Word8
-----------------------------------------------------------------------------

data Env = MkEnv { _worlds :: !(IORef [Context])
                 }
makeLenses ''Env

initEnv :: [Context] -> IO Env
initEnv ws = MkEnv <$> newIORef ws

printEnv :: Env -> IO ()
printEnv env = do
  ws <- readIORef (_worlds env)
  print ws

-- | It looks like the Selective combinators has mush have nothing to do with
--   symbolic execution per se, i.e. they must just execute both branches somehow,
--   like Over, and somehow forward the symbolic execution to the `read/write` callbacks.
--   Another thing: maybe read :: Bool needs to push into a queue and the path constraint
--   and it's negation and the two branches then will pop it and add to their list of path
--   conditions.

newtype Engine a = MkEngine {getEngine :: Env -> IO a }

instance Functor Engine where
  fmap f (MkEngine x) = MkEngine $ \e ->
    fmap f (x e)

instance Applicative Engine where
  pure x = MkEngine $ \_ -> pure x
  (<*>) = ap

instance Selective Engine where
  select = selectA

instance Monad Engine where
  (MkEngine x) >>= f = MkEngine $ \e -> do
    xRes <- x e
    run (f xRes) e

instance MonadIO Engine where
  liftIO action = MkEngine $ \_ -> action

instance (MonadReader Env) Engine where
  ask = MkEngine $ \env -> pure env
  local = error "MonadReader.local is not defined for Engine"

run :: Engine a -> Env -> IO a
run = getEngine

-----------------------------------------------------------------------------
dumpWorlds :: Env -> IO ()
dumpWorlds env = do
  print =<< readIORef (_worlds env)

pushWorld :: Context -> Engine ()
pushWorld ctx = MkEngine $ \env -> do
  modifyIORef (_worlds env) (\ws -> ctx:ws)

popWorld :: Engine Context
popWorld = MkEngine $ \env -> do
  ws <- readIORef (_worlds env)
  case ws of
    [] -> error "Engine.popWorld: the stack of worlds is empty!"
    (x:xs) -> do
      writeIORef (_worlds env) xs
      pure x

pickWorld :: Engine Context
pickWorld = MkEngine $ \env -> do
  ws <- readIORef (_worlds env)
  case ws of
    [] -> error "Engine.popWorld: the stack of worlds is empty!"
    (x:_) -> do
      pure x

{-
   TODO: Should (readIO IC) return Bool, i.e. return true if the onTrue branch is
   taken and False if onFalse? maybe this will help to create a proper
   Selective instance?
-}
readIO :: Key -> Engine SExpr
readIO = \case
  -- IC  -> do
  --   ctx <- popWorld
  --   let ic = fromJust $ Map.lookup IC (_bindings ctx)
  --       ctxOnTrue  = ctx {_pathCondition = sOp And [ic          , _pathCondition ctx]}
  --       ctxOnFalse = ctx {_pathCondition = sOp And [sOp Not [ic], _pathCondition ctx]}
  --   pushWorld ctxOnTrue
  --   pushWorld ctxOnFalse
  --   pure ic
  key -> do
    ctx <- _bindings <$> pickWorld
    case Map.lookup key ctx of
         Nothing  -> error $ "Undefined key: " <> show key
         Just val -> do
           liftIO $ putStrLn $ "Read: " <> show key
           pure val

writeIO :: Key -> Engine SExpr -> Engine SExpr
writeIO key producer = do
  value <- producer
  w <- popWorld
  let ctx = _bindings w
  pushWorld $ w {_bindings = Map.insert key value ctx}
  liftIO $ putStrLn $ "Write: (" <> show key <> ", " <> show value <> ")"
  pure value

ex :: IO ()
ex = do
  let ctx = MkContext { _pathCondition = sTrue
                      , _bindings = Map.fromList [ (IC, sConst 0)
                                                 , (Reg R0, sOp Plus [sConst 0, sConst 1])
                                                 , (Reg R1, sConst 1)
                                                 ] }
  env <- initEnv [ctx]
  (flip run) env $ do
    r0 <- readIO (Reg R0)
    -- liftIO . print $ r0
    r1 <- readIO (Reg R1)
    -- liftIO . print $ r1
    writeIO (Reg R1) (readIO (Reg R1))
  putStrLn "Final environment:"
  printEnv env
  print "bye"

-- -- | A generic symbolic execution mechanism is parameterised by three things:
-- --   * a function @pickNext@ for choosing the next state in the worklist
-- --   * a function @follow@ that returns a decision whether to follow a branch
-- --     or to prune it, i.e. performs a feasibility check
-- --   * a relation @~@ that controls whether states should be merged
