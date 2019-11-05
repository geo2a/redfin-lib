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
import           Data.Word            (Word8)
import           Lens.Micro.Platform
import           Prelude              hiding (log)

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

newtype Context = MkContext { _bindings :: Map.Map Key SExpr }

makeLenses ''Context

instance Show Context where
  show ctx = show (_bindings ctx)

type WorldId = Word8
-----------------------------------------------------------------------------

data Env = MkEnv { _worlds    :: !(IORef (Map.Map WorldId Context))
                 , _worldTree :: !(IORef (Tree WorldId))
                 , _worldId   :: !(IORef WorldId)
                 }

makeLenses ''Env

initEnv :: Map.Map WorldId Context -> IO Env
initEnv ws = do
  wsRef  <- newIORef ws
  wTree   <- newIORef Tree.empty
  widRef <- newIORef 0
  pure $ MkEnv wsRef wTree widRef

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
  wid <- readIORef (_worldId env)
  putStrLn $ "Last world id: " <> show wid
  ws <- readIORef (_worlds env)
  print ws

inc :: Engine ()
inc = do
  (MkContext w) <- getCurrentWorld
  -- pure ()
  putCurrentWorld (MkContext (Map.adjust (\x -> sOp Plus [x, sConst 1]) IC w))

test :: IO ()
test = do
  let firstWorld = MkContext $ Map.fromList [(IC, sConst 0)]
      ws = Map.fromList [(0, firstWorld)]
  env <- initEnv ws
  dumpWorlds env
  -- run forkWorld env
  run (whenSEngine (pure sTrue) inc) env
  -- run inc env
  dumpWorlds env

-----------------------------------------------------------------------------
-- | Increment the world counter and return the new world id;
--   Throw error if the maximum amount of worlds has been reached.
incWorldId :: Engine WorldId
incWorldId = MkEngine $ \env -> do
  wid <- readIORef (_worldId env)
  when (wid == maxBound) $
    let msg = "Engine.newWorldId: max world count " <> show wid <> " reached!"
    in error msg
  let wid' = wid + 1
  writeIORef (_worldId env) wid'
  pure wid'

getWorld :: WorldId -> Engine Context
getWorld wid = MkEngine $ \env -> do
  ws <- readIORef (_worlds env)
  case Map.lookup wid ws of
    Nothing    -> error $ "Engine.getWorld: no world with id " <> show wid
    Just world -> pure world

getCurrentWorld :: Engine Context
getCurrentWorld = MkEngine $ \env -> do
  wid <- readIORef (_worldId env)
  run (getWorld wid) env

putCurrentWorld :: Context -> Engine ()
putCurrentWorld ctx = MkEngine $ \env -> do
  wid <- readIORef (_worldId env)
  run (putWorld wid ctx) env

putWorld :: WorldId -> Context -> Engine ()
putWorld wid world = MkEngine $ \env ->
  modifyIORef (_worlds env) (\ws -> Map.insert wid world ws)

-- | Create a new possible world
forkWorld :: Engine ()
forkWorld = do
  currentWorld <- getCurrentWorld
  wid          <- incWorldId
  putWorld wid currentWorld

whenSEngine :: Engine SExpr -> Engine () -> Engine ()
whenSEngine cond comp = do
  t <- cond
  conditionSat <- liftIO $ sat t
  -- if the condition is unsatisfiable, we don't branch
  when (isUnsat conditionSat) $ pure ()
  -- otherwise, we clone the world and execute the computation in it
  forkWorld
  comp

-- -- biselect :: Engine (Either a b) -> Engine (Either a c) -> Engine (Either a (b, c))
-- -- biselect l r = Engine $ \e -> do

-- branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
-- branch x l r = fmap (fmap Left) x <*? fmap (fmap Right) l <*? r

-- ifS :: Selective f => f Bool -> f a -> f a -> f a
-- ifS x t e = branch (bool (Right ()) (Left ()) <$> x) (const <$> t) (const <$> e)

readIO :: Key -> Engine SExpr
readIO = \case
  IC  -> undefined
  key -> do
    ctx <- _bindings <$> getCurrentWorld
    case Map.lookup key ctx of
         Nothing  -> error $ "Undefined key: " <> show key
         Just val -> do
           liftIO $ putStrLn $ "Read: " <> show key
           pure val

-- -- -- solveBool :: SExpr ->

-- -- readFlag :: Key -> Engine (SExpr, Maybe Bool)
-- -- readFlag = \case
-- --   F flag -> do
-- --     boundKeys <- liftIO . readIORef . _bindings =<< ask
-- --     case Map.lookup (F flag) boundKeys of
-- --          Nothing  -> error $ "Undefined key: " <> show flag
-- --          Just val ->
-- --            pure (val, Just False)
-- --   _ -> error "readFlag: Not a flag key"

-- -- -- -- writeIO :: Key -> Engine SExpr -> Engine SExpr
-- -- -- -- writeIO key producer = do
-- -- -- --   env <- ask
-- -- -- --   value <- liftIO (runReaderT producer env)
-- -- -- --   env <- ask
-- -- -- --   boundKeys <- liftIO . readIORef . bindings $ env
-- -- -- --   liftIO $ writeIORef (bindings env) (Map.adjust (const value) key boundKeys)
-- -- -- --   liftIO $ putStrLn $ "Write: (" <> show key <> ", " <> show value <> ")"
-- -- -- --   pure value

-- -- -- -- ex :: IO ()
-- -- -- -- ex = do

-- -- -- --   print "bye"

-- -- -- -- -- | A generic symbolic execution mechanism is parameterised by three things:
-- -- -- --   * a function @pickNext@ for choosing the next state in the worklist
-- -- -- --   * a function @follow@ that returns a decision whether to follow a branch
-- -- -- --     or to prune it, i.e. performs a feasibility check
-- -- -- --   * a relation @~@ that controls whether states should be merged
