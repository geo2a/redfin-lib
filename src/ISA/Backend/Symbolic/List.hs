{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Symbolic.List
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Pure tree trace-based symbolic execution backend
-----------------------------------------------------------------------------
module ISA.Backend.Symbolic.List
    ( -- * symbolic execution state
      Context(..), showKey
      -- * symbolic execution engine
    , Engine (..)
      -- instances of read/write callbacks for Engine
    , readKey, writeKey
    ) where

import           Control.Monad.Reader
import           Control.Monad.State.Class
-- import           Control.Selective
import qualified Data.Map.Strict            as Map
import           Prelude                    hiding (log, not, read, readIO)
import           Unsafe.Coerce

import           ISA.Selective
import           ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context

-- | A Symbolic Execution Engine is a combination of State and List monads:
--   given a state, it produces a list of new possible states. Imagine conditional
--   branch: one state (before branch) produces two new states, one where the condition
--   is true and one where it's false.
--
--   A fun note: isn't this a monadic parser? How can we explore a connection between
--   symbolic execution and parsing and turn it into a functional pearl for ICFP...
data Engine a = Engine
    { runEngine :: Context -> [(a, Context)] }

-- | The 'Functor' instance for @Engine@
instance Functor Engine where
  fmap f e =
    Engine $ \s0 -> do
      (res, s) <- runEngine e s0
      pure (f res, s)

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative Engine where
    pure  = return
    (<*>) = ap
    (*>)  = (>>)

instance Selective Engine where
  select condition boolElim dataElim = Engine $ \s0 -> do
    (cond, s) <- runEngine condition s0
    case (trySolve . unsafeCoerce $ cond) of
      Trivial True  -> runEngine (boolElim True) s
      Trivial False -> runEngine (boolElim False) s
      Nontrivial symbolic ->
        let onTrue  = s { _pathCondition = SAnd symbolic (_pathCondition s) }
            onFalse = s { _pathCondition = SAnd (SNot symbolic) (_pathCondition s) }
        in runEngine (boolElim True *> (($ (unsafeCoerce symbolic)) <$> dataElim)) onTrue
           ++
           runEngine (boolElim False *> pure (unsafeCoerce symbolic)) onFalse
           -- [((unsafeCoerce symbolic) :: b, onFalse)]

-- | A 'Monad' instance for 'Engine' is a combination of state and list monads:
--   * 'return' embeds the value paired with the current state into a list
--   * '>>=' exploits the list monad to run 'f' on every result of 'computation' and
--     join the resulting lists of results.
instance Prelude.Monad Engine where
    return a       = Engine $ \s -> [(a, s)]
    Engine computation >>= f = Engine $ \s -> do
      (result, s') <- computation s
      runEngine (f result) s'

instance (MonadState Context) Engine where
    get   = Engine $ \s -> [(s, s)]
    put s = Engine $ \_ -> [((), s)]

readKey :: Key -> Engine (Data Sym)
readKey key = do
    ctx <- get
    let x = case Map.lookup key (_bindings ctx) of
              Just v -> v
              Nothing -> error $ "Engine.readKey: uninitialised key " <> show key
    pure (MkData x)

writeKey :: Key -> Engine (Data Sym) -> Engine (Data Sym)
writeKey key computation = do
  ctx <- get
  (MkData value) <- computation
  put $ ctx {_bindings = Map.insert key value (_bindings ctx)}
  pure (MkData value)
-----------------------------------------------------------------------------
