{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Context
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@geo2a.info
-- Stability  : experimental
--
-- A 'Context' is Map of bindings used in simulation
-----------------------------------------------------------------------------
module ISA.Types.Context
  ( Context(..), SymbolicContext, emptyCtx

  -- substitute pointers from @_store@ into @bindings@
  , substPointers

  -- extract memory from @_bindings@
  , dumpMemory

  , getBinding, putBinding, showKey, isReachable) where

import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Text                  (Text)
import           GHC.Generics

import           ISA.Types
import           ISA.Types.Key
import           ISA.Types.Prop
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address

-- | A record type for state of the (symbolically) simulated computation.
--   The type variable may be instantiated with 'Sym' for symbolic simulation, see 'ISA.Types.SymbolicContext'
data Context a = MkContext {
  -- | keys (like register names, memory cells) mapped to their (symbolic) values
  _bindings        :: Map.Map Key a
  -- | A store used for tracking symbolic points-to
  , _store         :: Map.Map Text a
  -- | a boolean formula which must be satisfiable for this state to be reachable
  , _pathCondition :: a
  -- | a list of named boolean formulas, mostly used as preconditions and conjoined with @_pathCondition@
  --   when checking Reachability
  , _constraints   :: [(Text, a)]
  -- | a response from a solver, usually regarding
  --   satisfiability of @_pathCondition s && conjoin (_constraints s)@
  , _solution      :: Maybe SMTResult }
  deriving (Functor, Generic) -- , ToJSON, FromJSON)

-- | Symbolic simulation is done with symbolic values
type SymbolicContext = Context Sym

-- | An empty context
emptyCtx :: Boolean a => Context a
emptyCtx = MkContext Map.empty Map.empty true [] Nothing

dumpMemory :: Context a -> [(Address, a)]
dumpMemory = catMaybes . map (uncurry getAddr) . Map.assocs . _bindings
  where getAddr (Addr a) v = Just (a, v)
        getAddr _ _        = Nothing

instance Eq a => Eq (Context a) where
  x == y = (_bindings x == _bindings y)
        && (_store x == _store y)
        && (_constraints x == _constraints y)

-- | A context is reachable if it's '_solution' is present and satisfiable
isReachable :: Context a -> Bool
isReachable ctx = case (_solution ctx) of
  Nothing              -> True
  Just (Satisfiable _) -> True
  _                    -> False

-- | Access a specific key
getBinding :: Key -> Context a -> Maybe a
getBinding key ctx = Map.lookup key (_bindings ctx)

-- | Alter a specific key
putBinding :: Key -> a -> Context a -> Context a
putBinding key v ctx = ctx {_bindings = Map.insert key v (_bindings ctx)}

getVar :: Text -> Context a -> Maybe a
getVar key ctx = Map.lookup key (_store ctx)

substPointers :: Context (Data Sym) -> Context (Data Sym)
substPointers ctx =
  let ptrs = _store ctx
  in foldr (uncurry substPointer) ctx (Map.assocs ptrs)

substPointer :: Text -> Data Sym -> Context (Data Sym) -> Context (Data Sym)
substPointer pname (MkData expr) ctx =
  ctx {_bindings = fmap (subst expr pname <$>) (_bindings ctx)}

showKey :: Show a => Context a -> Key -> String
showKey ctx key =
  case Map.lookup key (_bindings ctx) of
    Nothing -> "uninitialised"
    Just v  -> show v

instance Show a => Show (Context a) where
  show ctx = unlines [ "Path constraint: " <> show (_pathCondition ctx)
                     , showKey ctx IR
                     ]
