{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Backend.Symbolic
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- A simple symbolic execution back-end
-- inspired by [SBV](http://hackage.haskell.org/package/sbv)
-- but more fine-tuned for executing low-level code
--
-----------------------------------------------------------------------------
module ToyRISC.Backend.Symbolic
    ( Env(..), Engine (..), run
    , State (..), bindings
    , pathCond, model -- lenses for State
    , ) where

-- import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Selective
import           Data.IORef
import qualified Data.Map             as Map
import           Lens.Micro.Platform

-- import           ToyRISC.SMT
import           ToyRISC.Symbolic
import           ToyRISC.Types

-- | State of the symbolic interpreter
data State modelTy = State
              { _pathCond :: SExpr
              -- ^ path condition: a list of symbolic expressions of kind Bool
              , _model    :: modelTy
              -- ^ the state of the runtime being modelled, i.e. processor or
              --   virtual machine
              } deriving (Show, Eq)

makeLenses ''State

data Env = MkEnv { _bindings :: IORef (Map.Map Key SExpr) }

makeLenses ''Env

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
  pure = return
  (<*>) = ap

instance Monad Engine where
  x >>= f = MkEngine $ \e -> do
    xRes <- (getEngine x) e
    getEngine (f xRes) e

run :: Engine a -> Env -> IO a
run comp = getEngine comp

instance Selective Engine where
  select = selectA

-- biselect :: Engine (Either a b) -> Engine (Either a c) -> Engine (Either a (b, c))
-- biselect l r = Engine $ \e -> do

-- whenSEngine :: Engine SExpr -> Engine () -> Engine ()
-- whenSEngine cond comp = MkEngine $ \env -> do
--   condition <- solveBool =<< run cond env

--   pure ()
-----------------------------------------------------------------------------

data Two a = MkTwo a a
  deriving (Show, Eq)

instance Functor Two where
  fmap f (MkTwo x y) = MkTwo (f x) (f y)

instance Applicative Two where
  pure x = MkTwo x x
  -- f <*> MkTwo x y = MkTwo

instance Monad Two where
  (MkTwo x y) >>= f =
    let x' = f x
        y' = f y
    in undefined

-- branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
-- branch x l r = fmap (fmap Left) x <*? fmap (fmap Right) l <*? r

-- ifS :: Selective f => f Bool -> f a -> f a -> f a
-- ifS x t e = branch (bool (Right ()) (Left ()) <$> x) (const <$> t) (const <$> e)

-- whenS :: Selective f => f Bool -> f () -> f ()
-- whenS x y = select (bool (Right ()) (Left ()) <$> x) (const <$> y)

-- readIO :: Key -> Engine SExpr
-- readIO key = do
--   boundKeys <- liftIO . readIORef . _bindings =<< ask
--   case Map.lookup key boundKeys of
--        Nothing  -> error $ "Undefined key: " <> show key
--        Just val -> do
--          liftIO $ putStrLn $ "Read: " <> show key
--          pure val

-- -- solveBool :: SExpr ->

-- readFlag :: Key -> Engine (SExpr, Maybe Bool)
-- readFlag = \case
--   F flag -> do
--     boundKeys <- liftIO . readIORef . _bindings =<< ask
--     case Map.lookup (F flag) boundKeys of
--          Nothing  -> error $ "Undefined key: " <> show flag
--          Just val ->
--            pure (val, Just False)
--   _ -> error "readFlag: Not a flag key"

-- -- -- writeIO :: Key -> Engine SExpr -> Engine SExpr
-- -- -- writeIO key producer = do
-- -- --   env <- ask
-- -- --   value <- liftIO (runReaderT producer env)
-- -- --   env <- ask
-- -- --   boundKeys <- liftIO . readIORef . bindings $ env
-- -- --   liftIO $ writeIORef (bindings env) (Map.adjust (const value) key boundKeys)
-- -- --   liftIO $ putStrLn $ "Write: (" <> show key <> ", " <> show value <> ")"
-- -- --   pure value

-- -- -- ex :: IO ()
-- -- -- ex = do

-- -- --   print "bye"

-- -- -- -- | A generic symbolic execution mechanism is parameterised by three things:
-- -- --   * a function @pickNext@ for choosing the next state in the worklist
-- -- --   * a function @follow@ that returns a decision whether to follow a branch
-- -- --     or to prune it, i.e. performs a feasibility check
-- -- --   * a relation @~@ that controls whether states should be merged
