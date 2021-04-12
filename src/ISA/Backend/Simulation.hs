{- |
 Module     : ISA.Backend.Simulation
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Forwarding semantics reads and writes onto a polymorphic key-value context

 The type variable 'a' of the context may be instantiated with 'ISA.Types.Symbolic.Sym' for
 symbolic simulation, or with other types for concrete execution and various
 analyses.
-}
module ISA.Backend.Simulation (
    Simulate,
    simulate,
    simulateRead,
    simulateWrite,
    Missing (..),
    missing,
) where

import qualified Data.Map as Map
import Polysemy
import Polysemy.Error
import Polysemy.State

import ISA.Types.Context
import ISA.Types.Key

-- | An exception type to track uninitialised keys
newtype Missing key = MkMissing key
    deriving (Show, Eq)

missing :: key -> Missing key
missing = MkMissing

-- | A type alias for the effect needed for simulation
type Simulate s a = Sem [State (Context s), Error (Missing Key)] a

-- | Run simulation on an initial state with missing keys initialised with @missingDefault@
simulate :: b -> Context a -> Simulate a b -> b
simulate missingDefault init computation =
    run
        . fmap (either (const missingDefault) id)
        . runError @(Missing Key)
        . evalState init
        $ computation

-- | Simulate reads from a polymorphic 'ISA.Types.Context'.
simulateRead :: Key -> Simulate a a
simulateRead key =
    Map.lookup key . _bindings <$> get >>= \case
        Just v -> pure v
        Nothing -> throw (missing key)

-- | Simulate writes to a polymorphic 'ISA.Types.Context'.
simulateWrite :: Key -> Simulate a a -> Simulate a a
simulateWrite key fv = do
    val <- fv
    modify (\ctx -> ctx{_bindings = Map.insert key val (_bindings ctx)})
    pure val
