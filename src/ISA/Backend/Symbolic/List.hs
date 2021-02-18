{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Symbolic.List
-- Copyright  : (c) Georgy Lukyanov 2020-2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Symbolic simulation backend using Data.Tree from containers
-----------------------------------------------------------------------------
module ISA.Backend.Symbolic.List
    ( -- * symbolic execution engine
      Engine (..)
      -- * instances of read/write callbacks for Engine
    , readKey, writeKey
    ) where

import           Control.Monad.Reader
import           Control.Monad.State.Class
import qualified Data.Map.Strict           as Map
import           Prelude                   hiding (log, not, read, readIO)
import           Unsafe.Coerce

import           ISA.Selective
import           ISA.Types
import           ISA.Types.Context         hiding (Context)
import qualified ISA.Types.Context         as ISA.Types
import           ISA.Types.Symbolic

type Context = ISA.Types.Context Sym

-- | A Symbolic Execution Engine is a combination of State and List monads:
--   given a state, it produces a list of new possible states. Imagine conditional
--   branch: one state (before branch) produces two new states, one where the condition
--   is true and one where it's false.
--
--   A fun note: isn't this a monadic parser? How can we explore a connection between
--   symbolic execution and parsing and turn it into a functional pearl for ICFP...
data Engine a = Engine { runEngine :: Context -> IO [(a, Context)] }

-- | The 'Functor' instance for @Engine@
instance Functor Engine where
  fmap f e =
    Engine $ \s0 -> do
      xs <- runEngine e s0
      forM xs $ \(res, s) ->
        pure (f res, s)

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative Engine where
    pure x = Engine $ \s -> pure [(x, s)]
    ff <*> fa = Engine $ \s -> do
                  fs <- runEngine ff s
                  results <-
                    forM fs $ \(f, s') -> do
                      as <- runEngine fa s'
                      forM as $ \(a, s'') -> pure (f a, s'')
                  pure . concat $ results

decide :: (Prop Sym, Context) -> (Bool -> Engine Sym) -> (Engine (Sym -> Sym))
  -> IO [(Sym, Context)]
decide (cond, s) boolElim dataElim =
  case trySolve cond of
    Trivial True  -> runEngine (boolElim True) s
    Trivial False -> runEngine (boolElim False) s
    Nontrivial symbolic ->
      let onTrue  = s { _pathCondition = SAnd symbolic (_pathCondition s) }
          onFalse = s { _pathCondition = SAnd (SNot symbolic) (_pathCondition s) }
      in do r <- runEngine (boolElim True *> (($ symbolic) <$> dataElim)) onTrue
            l <- runEngine (boolElim False *> pure symbolic) onFalse
            pure (l ++ r)

instance Selective Engine where
  select condition boolElim dataElim = Engine $ \s0 -> unsafeCoerce $ do
    xs <- runEngine condition s0
    concat <$> forM xs (\(cond, ctx) ->
                          decide (unsafeCoerce cond, ctx)
                                 (unsafeCoerce boolElim) (unsafeCoerce dataElim))

-- | A 'Monad' instance for 'Engine' is a combination of state and list monads:
--   * 'return' embeds the value paired with the current state into a list
--   * '>>=' exploits the list monad to run 'f' on every result of 'computation' and
--     join the resulting lists of results.
instance Prelude.Monad Engine where
  return       = pure
  Engine computation >>= f = Engine $ \s -> do
    xs <- computation s
    ys <- forM xs $ \(x, s') -> runEngine (f x) s'
    pure $ concat ys

instance (MonadState Context) Engine where
  get   = Engine $ \s -> pure [(s, s)]
  put s = Engine $ \_ -> pure [((), s)]

readKey :: Key -> Engine (Data Sym)
readKey key = do
  ctx <- get
  let x = case Map.lookup key (_bindings ctx) of
            Just v  -> v
            Nothing -> error $ "Engine.readKey: uninitialised key " <> show key
  pure (MkData x)

writeKey :: Key -> Engine (Data Sym) -> Engine (Data Sym)
writeKey key computation = do
  ctx <- get
  (MkData value) <- computation
  put $ ctx {_bindings = Map.insert key value (_bindings ctx)}
  pure (MkData value)
-----------------------------------------------------------------------------
