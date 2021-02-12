{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Symbolic.QueryList
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Symbolic simulation over a zipper-focused binary tree
-----------------------------------------------------------------------------
module ISA.Backend.Symbolic.Zipper
  ( -- * symbolic simulation engine
    Engine, execEngine
  , EngineState(..), effectOnContext
  -- * symbolic simulation trace
  , OneTwo(..), Trace (..), getFocused, putFocused, growTrace
    -- * Zipper interface for _trace of the Engine
  , left, up, right, down
    -- * instances of read/write callbacks for Engine
  , readKey, writeKey
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Selective
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Data.SBV.Trans             as SBV
import           GHC.Stack
import           Prelude                    hiding (log, not, read, readIO)

import           ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Tree             hiding (down, left, right, up)
import qualified ISA.Types.Tree             as Tree

-- | Trace of a symbolic simulation
data Trace =
  MkTrace { _layout :: Tree Int ()
         -- ^ Binary tree of state identifiers
          , _focus  :: Loc Int ()
         -- ^ zipper for _layout
          , _states :: IntMap Context
         -- ^ the actual states
          }

-- | Choice between a single child node and a branch
data OneTwo a = One a | Two a a
  deriving (Show, Functor)

-- | State of the symbolic engine
data EngineState = MkEngineState
  { _trace       :: TMVar Trace
 -- ^ symbolic simulation trace
  , _choice      :: TMVar (Maybe (Context, Context))
 -- ^ branching instructions will fill this variable with
 --   a branch via Engine's Selective instance
  , _statesCount :: TMVar Int
 -- ^ the number of (reachable) states discovered so far,
 --   i.e. the size of _states in _trace
  }

-- | The symbolic simulation engine is a reader monad of the mutable
--   environment and a state monad of the trace's zipper
newtype Engine a = MkEngine (SBV.SymbolicT (ReaderT EngineState IO) a)
  deriving ( Functor, Applicative, Monad
           , MonadReader EngineState, MonadIO
           , SBV.MonadSymbolic)

-- | Hook into the mutable environment to get a transparent access to the zipper
instance MonadState (Loc Int ()) Engine where
  get = do
    env <- ask
    trace <- liftIO . atomically . readTMVar . _trace $ env
    pure (_focus trace)
  put focus = do
    env <- ask
    liftIO . atomically $ do
      trace <- takeTMVar (_trace env)
      putTMVar (_trace env) $ trace {_focus = focus}

-- | Engine is a Selective functor
instance Selective Engine where
  select = selectEngine

-- | The select implementation for Engine
selectEngine :: Engine (Either a b) -> Engine (a -> b) -> Engine b
selectEngine choice onLeft = do
  c <- choice
  ctx <- getFocused
  let cond = fromJust $ getBinding (F Condition) ctx
  let yes = ctx {_pathCondition = SAnd cond (_pathCondition ctx)}
      no  = ctx {_pathCondition = SAnd (SNot cond) (_pathCondition ctx)}
  (r, yes') <- effectOnContext yes (either (\x -> ($ x) <$> onLeft)
                                           (\x -> onLeft *> pure x) c)
  makeChoice yes' no
  pure r

-- | Fill _choice with two new children nodes
makeChoice :: Context -> Context -> Engine ()
makeChoice ctx1 ctx2 = do
  env <- ask
  void . liftIO . atomically $ do
    swapTMVar (_choice env) (Just (ctx1, ctx2))

-- | Perform a computation to snapshot it's effect on the mutable state, and backtrack the state
effectAndBacktrack :: Engine a -> Engine a
effectAndBacktrack effect = do
  env <- ask
  traceBefore <- liftIO . atomically $ readTMVar (_trace env)
  result <- effect
  void . liftIO . atomically $ do
    swapTMVar (_trace env) traceBefore
  pure result

-- | Execute a computation in the given state to observe its effect,
--   backtrack the state to the one before the execution and
--   return the observed changed state (and the result)
effectOnContext :: Context -> Engine a -> Engine (a, Context)
effectOnContext ctx effect =
  effectAndBacktrack $ do
    putFocused ctx
    r <- effect
    ctx' <- getFocused
    pure (r, ctx')

-- | Execute an Engine computation from the given initial state
execEngine :: Engine a -> Context -> IO Trace
execEngine (MkEngine computation) initialContext = do
  let layout = Leaf 0 ()
      initStates = IntMap.fromList [(0, initialContext)]
  env <- MkEngineState <$> newTMVarIO (MkTrace layout (Loc layout Top) initStates)
                       <*> newTMVarIO Nothing
                       <*> newTMVarIO 0
  void $ runReaderT (SBV.runSMT computation) env
  liftIO . atomically $ readTMVar (_trace env)

-- | Zip into left subtrace
left :: Engine ()
left = do
  (Loc tree cxt) <- get
  put $ case tree of
          Branch n lChild rChild -> Loc lChild (L n cxt rChild)
          _                      -> Loc tree cxt

-- | Zip into right subtrace
right :: Engine ()
right = do
  (Loc tree cxt) <- get
  case tree of
     Branch n lChild rChild -> put $ Loc rChild (R n lChild cxt)
     _                      -> put $ Loc tree cxt

-- | Zip up the trunk of to the joint of a branch
up :: Engine ()
up = do
  (Loc t cxt) <- get
  case cxt of
    D n c   -> put $ Loc (Trunk n t) c
    L n c r -> put $ Loc (Branch n t r) c
    R n l c -> put $ Loc (Branch n l t) c
    _       -> put (Loc t cxt)

-- | Zip down the trunk
down :: Engine ()
down = do
  (Loc t cxt) <- get
  case t of
    Trunk n child -> put $ Loc child (D n cxt)
    _             -> put $ Loc t cxt

-- | Get the state focused by the trace zipper
getFocused :: HasCallStack => Engine Context
getFocused = do
  trace <- liftIO . atomically . readTMVar =<< _trace <$> ask
  focus <- get
  let k = locKey focus
  case IntMap.lookup k (_states trace) of
    Nothing -> error $ "No such state " <> show k
    Just x  -> pure x

-- | Put a state a trace zipper's focus
putFocused :: Context -> Engine ()
putFocused ctx = do
  traceVar <- _trace <$> ask
  void . liftIO . atomically $ do
    trace <- takeTMVar traceVar
    putTMVar traceVar $
      trace { _states = IntMap.insert (locKey (_focus trace)) ctx (_states trace) }

-- | Add a branch or grow the trunk of the trace without moving
--   zipper's focus, i.e. the focus will stay at the parent node of
--   the newly added node/nodes
growTrace :: Maybe (OneTwo Context) -> Engine ()
growTrace Nothing = pure ()
growTrace (Just choice) = do
  focus <- get
  env <- ask
  (trace, size) <- liftIO . atomically $
    (,) <$> takeTMVar (_trace env) <*> takeTMVar (_statesCount env)
  let father = locKey focus
  let withKeys =
        case choice of
          One x   -> One (size + 1, x)
          Two x y -> Two (size + 1, x) (size + 2, y)
  void . liftIO . atomically $ do
    putTMVar (_trace env)
      trace { _layout = fst $ updateLayout father focus withKeys
            , _focus = snd $ updateLayout father focus withKeys
            , _states = updateStates (_states trace) withKeys
            }
    putTMVar (_statesCount env) (updateStatesCount size choice)
  where
    updateLayout :: Int -> Loc Int () -> OneTwo (Int, Context) -> (Tree Int (), Loc Int ())
    updateLayout father to = \case
      One (k, _) ->
        let subtree = Tree.putTree (Trunk father (Leaf k ()))
        in ( Tree.travel to (subtree *> Tree.top *> Tree.getTree)
           , Tree.shift to subtree
           )
      Two (k1, _) (k2, _) ->
        let subtree = Tree.putTree (Branch father (Leaf k1 ())
                                                  (Leaf k2 ()))
        in ( Tree.travel to (subtree *> Tree.top *> Tree.getTree)
           , Tree.shift to subtree
           )

    updateStates :: IntMap Context -> OneTwo (Int, Context) -> IntMap Context
    updateStates states = \case
      One (k, ctx) -> IntMap.insert k ctx states
      Two (k1, ctx1) (k2, ctx2) -> IntMap.insert k2 ctx2
                                   (IntMap.insert k1 ctx1 states)

    updateStatesCount :: Int -> OneTwo Context -> Int
    updateStatesCount s = \case One _ -> s + 1
                                Two _ _ -> s + 2

-- | The read callback to plug into FS semantics of instructions
readKey :: HasCallStack => Key -> Engine (Data Sym)
readKey key = do
    ctx <- getFocused
    let x = case Map.lookup key (_bindings ctx) of
              Just v  -> v
              Nothing -> defaultFor key
    pure (MkData x)
  where
    defaultFor :: Key -> Sym
    defaultFor = \case
      Reg _ -> 0
      F _ -> 0
      Addr _ -> 0
      IR -> 0
      IC -> -1
      Prog _ -> 0

-- | The write callback to plug into FS semantics of instructions
writeKey :: HasCallStack => Key -> Engine (Data Sym) -> Engine (Data Sym)
writeKey key computation = do
  (MkData value) <- computation
  ctx <- getFocused
  putFocused $ ctx {_bindings = Map.insert key value (_bindings ctx)}
  pure (MkData value)
