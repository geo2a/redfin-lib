{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
 Module     : ISA.Types.Context
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@geo2a.info
 Stability  : experimental

 A 'Context' is Map of bindings used in simulation
-}
module ISA.Types.Context (
    -- * Simulator state
    Context (..),
    emptyCtx,

    -- ** substitute pointers from @_store@ into @bindings@
    substPointer,
    substPointers,
    unstar,
    unstarAll,

    -- ** extract memory from @_bindings@
    dumpMemory,
    getBinding,
    putBinding,
    showKey,
    isReachable,
) where

import Control.DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import GHC.Generics

import ISA.Types.Boolean
import ISA.Types.Key
import ISA.Types.SBV
import ISA.Types.Symbolic
import ISA.Types.Symbolic.Address

{- | A record type for state of the (symbolically) simulated computation.
     The type variable may be instantiated with 'Sym' for symbolic simulation,
     see 'ISA.Types.SymbolicContext'
-}
data Context a = MkContext
    { -- | keys (like register names, memory cells) mapped to their (symbolic) values
      _bindings :: Map.Map Key a
    , -- | A store used for tracking symbolic points-to
      _store :: Map.Map Text a
    , -- | a boolean formula which must be satisfiable for this state to be reachable
      _pathCondition :: a
    , -- | a list of named boolean formulas, mostly used as preconditions and conjoined with @_pathCondition@
      --   when checking Reachability
      _constraints :: [(Text, a)]
    , -- | a response from a solver, usually regarding
      --   satisfiability of @_pathCondition s && conjoin (_constraints s)@
      _solution :: Maybe SMTResult
    }
    deriving (Functor, Generic, NFData)

instance Aeson.FromJSON a => Aeson.FromJSON (Context a)
instance Aeson.ToJSON a => Aeson.ToJSON (Context a)

-- | An empty context
emptyCtx :: Boolean a => Context a
emptyCtx = MkContext Map.empty Map.empty true [] Nothing

dumpMemory :: Context a -> [(Address, a)]
dumpMemory = catMaybes . map (uncurry getAddr) . Map.assocs . _bindings
  where
    getAddr (Addr a) v = Just (a, v)
    getAddr _ _ = Nothing

instance Eq a => Eq (Context a) where
    x == y =
        (_bindings x == _bindings y)
            && (_store x == _store y)
            && (_constraints x == _constraints y)

-- | A context is reachable if it's '_solution' is present and satisfiable
isReachable :: Context a -> Bool
isReachable ctx = case (_solution ctx) of
    Nothing -> True
    Just (Satisfiable _) -> True
    _ -> False

-- | Access a specific key
getBinding :: Key -> Context a -> Maybe a
getBinding key ctx = Map.lookup key (_bindings ctx)

unstar :: Context Sym -> Sym -> Maybe Sym
unstar ctx = \case
    SPointer a ->
        case toCAddress a of
            Left _ -> Nothing
            Right c ->
                let z = Map.lookup (Addr (MkAddress (Left c))) (_bindings ctx)
                 in z
    n@(SAny _) -> Just n
    n@(SConst _) -> Just n
    (SIte i t e) -> SIte <$> (unstar ctx i) <*> (unstar ctx t) <*> (unstar ctx e)
    (SAdd p q) -> SAdd <$> (unstar ctx p) <*> (unstar ctx q)
    (SSub p q) -> SSub <$> (unstar ctx p) <*> (unstar ctx q)
    (SMul p q) -> SMul <$> (unstar ctx p) <*> (unstar ctx q)
    (SDiv p q) -> SDiv <$> (unstar ctx p) <*> (unstar ctx q)
    (SMod p q) -> SMod <$> (unstar ctx p) <*> (unstar ctx q)
    (SAnd p q) -> SAnd <$> (unstar ctx p) <*> (unstar ctx q)
    (SOr p q) -> SOr <$> (unstar ctx p) <*> (unstar ctx q)
    (SEq p q) -> SEq <$> (unstar ctx p) <*> (unstar ctx q)
    (SGt p q) -> SGt <$> (unstar ctx p) <*> (unstar ctx q)
    (SLt p q) -> SLt <$> (unstar ctx p) <*> (unstar ctx q)
    (SAbs x) -> SAbs <$> (unstar ctx x)
    (SNot x) -> SNot <$> (unstar ctx x)

unstarAll :: Context Sym -> Context Sym
unstarAll ctx =
    ctx{_bindings = fmap (\v -> maybe 0 id $ unstar ctx v) (_bindings ctx)}

-- | Alter a specific key
putBinding :: Key -> a -> Context a -> Context a
putBinding key v ctx = ctx{_bindings = Map.insert key v (_bindings ctx)}

substPointers :: Context Sym -> Context Sym
substPointers ctx =
    let ptrs = _store ctx
     in foldr (uncurry substPointer) ctx (Map.assocs ptrs)

substPointer :: Text -> Sym -> Context Sym -> Context Sym
substPointer pname expr ctx =
    ctx{_bindings = fmap (subst expr pname) (_bindings ctx)}

showKey :: Show a => Context a -> Key -> String
showKey ctx key =
    case Map.lookup key (_bindings ctx) of
        Nothing -> "uninitialised"
        Just v -> show v

instance Show a => Show (Context a) where
    show ctx =
        unlines
            [ "Path constraint: " <> show (_pathCondition ctx)
            , showKey ctx IR
            ]
