-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Simulation
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Forwarding semantics reads and writes onto a polymorphic key-value context
--
-- The type variable 'a' of the context may be instantiated with 'ISA.Types.Symbolic.Sym' for
-- symbolic simulation, or with other types for concrete execution and various
-- analyses.
-----------------------------------------------------------------------------

module ISA.Backend.Simulation
  (simulateRead, simulateWrite
  , Missing(..), miss
  , Ignored(..), ignore
  ) where

import qualified Data.Map          as Map
import           Polysemy
import           Polysemy.Error
import           Polysemy.State

import           ISA.Types
import           ISA.Types.Context

-- | An exception type to track uninitialised keys
newtype Missing key = MkMissing key
  deriving (Show, Eq)

miss :: key -> Missing key
miss = MkMissing

-- | An exception type to track ignored keys
newtype Ignored key = MkIgnored key
  deriving (Show, Eq)

ignore :: key -> Ignored key
ignore = MkIgnored

-- | Simulate reads from a polymorphic 'ISA.Types.Context'.
simulateRead :: ( Member (State (Context a)) r
                , Member (Error (Missing Key)) r
                ) => Key -> Sem r a
simulateRead key =
    Map.lookup key . _bindings <$> get >>= \case
      Just v  -> pure v
      Nothing -> throw (miss key)

-- | Simulate writes to a polymorphic 'ISA.Types.Context'.
simulateWrite :: ( Member (State (Context a)) r
                 ) => Key -> Sem r a -> Sem r a
simulateWrite key fv = do
  val <- fv
  modify (\ctx -> ctx {_bindings = Map.insert key val (_bindings ctx)})
  pure val
