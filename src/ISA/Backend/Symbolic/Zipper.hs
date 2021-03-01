{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Symbolic.QueryList
-- Copyright  : (c) Georgy Lucknow 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Symbolic simulation over a zipper-focused binary tree
-----------------------------------------------------------------------------
module ISA.Backend.Symbolic.Zipper where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import qualified Control.Monad.Trans.Writer.CPS as Writer.CPS
import           Control.Monad.Writer.Class
import           Control.Selective
import           Data.IntMap.Strict             (IntMap)
import qualified Data.IntMap.Strict             as IntMap
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import qualified Data.SBV.Trans                 as SBV
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           GHC.Stack
import           ISA.Backend.Dependencies
import           Prelude                        hiding (log, not, read, readIO)

import           ISA.Types
import           ISA.Types.Context              hiding (Context)
import qualified ISA.Types.Context              as ISA.Types
import           ISA.Types.Key
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address
import           ISA.Types.Tree                 hiding (down, left, right, up)
import qualified ISA.Types.Tree                 as Tree
import           ISA.Types.ZeroOneTwo

type Context = ISA.Types.Context (Data Sym)

-- | Trace of a symbolic simulation
data Trace =
  MkTrace { _layout :: Tree Int ()
          -- ^ Binary tree of state identifiers
          , _focus  :: Loc Int ()
          -- ^ zipper for _layout
          , _states :: IntMap Context
          -- ^ the actual states
          }

-- | State of the symbolic engine
data EngineState = MkEngineState
  { -- | symbolic simulation trace
    _trace        :: TMVar Trace
    -- | the number of (reachable) states discovered so far,
    --   i.e. the size of _states in _trace
  , _statesCount  :: TMVar Int
  , _varCount     :: TVar Int
  -- | For how much steps to simplify expressions on write
  , _simplifyFuel :: Maybe Int
  }

freshStoreAddr :: Engine Text
freshStoreAddr = do
  s <- ask
  n <- liftIO . atomically $ modifyTVar (_varCount s) (+ 1)
  pure ("a" <> (Text.pack $ show n))

freshVarName :: Engine Text
freshVarName = do
  s <- ask
  n <- liftIO . atomically $ modifyTVar (_varCount s) (+ 1)
  pure ("v" <> (Text.pack $ show n))

remember :: Text -> Data Sym -> Engine ()
remember name expr = do
  ctx <- getFocused
  putFocused $ ctx {_store = Map.insert name expr (_store ctx) }

-- | The symbolic simulation engine is a reader monad of the mutable
--   environment and a state monad of the trace's zipper
newtype Engine a =
  MkEngine (SBV.SymbolicT (ReaderT EngineState IO) a)
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
selectEngine = selectM

-- | Execute an Engine computation from the given initial state
execEngine :: Engine a -> Context -> IO Trace
execEngine (MkEngine computation) initialContext = do
  let layout = Leaf 0 ()
      initStates = IntMap.fromList [(0, initialContext)]
  env <- MkEngineState <$> newTMVarIO (MkTrace layout (Loc layout Top) initStates)
                       <*> newTMVarIO 0
                       <*> newTVarIO 0
                       <*> pure (Just 100)
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
growTrace :: ZeroOneTwo Context -> Engine ()
growTrace choice = do
  focus <- get
  env <- ask
  (trace, size) <- liftIO . atomically $
    (,) <$> takeTMVar (_trace env) <*> takeTMVar (_statesCount env)
  let father = locKey focus
  let withKeys =
        case choice of
          Zero    -> Zero
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
    updateLayout :: Int -> Loc Int () -> ZeroOneTwo (Int, Context) -> (Tree Int (), Loc Int ())
    updateLayout father to = \case
      Zero -> (Tree.travel to (Tree.getTree), to)
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

    updateStates :: IntMap Context -> ZeroOneTwo (Int, Context) -> IntMap Context
    updateStates states = \case
      Zero  -> states
      One (k, ctx) -> IntMap.insert k ctx states
      Two (k1, ctx1) (k2, ctx2) -> IntMap.insert k2 ctx2
                                   (IntMap.insert k1 ctx1 states)

    updateStatesCount :: Int -> ZeroOneTwo Context -> Int
    updateStatesCount s = \case
      Zero    -> s
      One _   -> s + 1
      Two _ _ -> s + 2

-- | The read callback to plug into FS semantics of instructions
readKey :: HasCallStack => Key -> Engine (Data Sym)
readKey key = do
--  tell (Reads [key], Writes [])
  ctx <- getFocused
  x <-
    case key of
      (Addr (MkAddress (Left concrete))) ->
        pure $ Map.lookup (Addr (MkAddress (Left concrete))) (_bindings ctx)
      (Addr (MkAddress (Right sym))) -> do
        addr <- freshStoreAddr
        var <- freshVarName
        remember addr (MkData sym)
        pure $ Just (MkData $ SMapsTo (SAny addr) (SAny var))
      (Prog (MkAddress (Left concrete))) ->
        pure $ Map.lookup (Prog (MkAddress (Left concrete))) (_bindings ctx)
      (Prog (MkAddress (Right sym))) -> do
        addr <- freshStoreAddr
        var <- freshVarName
        remember addr (MkData sym)
        pure $ Just (MkData $ SMapsTo (SAny addr) (SAny var))
      _ -> pure $ Map.lookup key (_bindings ctx)
  pure (maybe (defaultFor key) id x)
  where
    defaultFor :: Key -> Data Sym
    defaultFor key = error $ "ISA.Backend.Symbolic.Zipper.readKey: the key "
                  <> show key <> " is not bound!"

-- | The write callback to plug into FS semantics of instructions
writeKey :: HasCallStack => Key -> Engine (Data Sym) -> Engine (Data Sym)
writeKey key computation = do
--  tell (Reads [], Writes [key])
  fuel <- _simplifyFuel <$> ask
  value <- (simplify fuel <$>) <$> computation
  ctx <- getFocused
  putFocused $ ctx {_bindings = Map.insert key value (_bindings ctx)}
  pure value
