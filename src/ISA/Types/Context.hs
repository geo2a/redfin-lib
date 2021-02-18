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
  ( Context(..), emptyCtx
  , getBinding, showKey, isReachable) where

import           Data.Aeson                   (FromJSON, ToJSON, defaultOptions,
                                               genericToEncoding, toEncoding)
import qualified Data.Map.Strict              as Map
import qualified Data.SBV                     as SBV
import           Data.Text                    (Text)
import           GHC.Generics

import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.SBV
import           ISA.Types.Symbolic

-- | A record type for state of the (symbolically) simulated computation.
--   The type variable may be instantiated with 'Sym' for symbolic simulation
--   * '_bindings': keys (like register names, memory cells) mapped to their (symbolic) values
--   * '_pathCondition' : an expression which must hold for this state to be
--     reachable
data Context a = MkContext { _bindings      :: Map.Map Key a
                           , _pathCondition :: a
                           , _constraints   :: [(Text, a)]
                           , _solution      :: Maybe SMTResult
                           }
  deriving (Generic, ToJSON, FromJSON)

-- | A context with defaults
emptyCtx :: (Boolean a, Num a) => Context a
emptyCtx = MkContext (Map.fromList [(IC, 0)]) true [] Nothing

instance Eq a => Eq (Context a) where
  x == y = (_bindings x == _bindings y)
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

showKey :: Show a => Context a -> Key -> String
showKey ctx key =
  case Map.lookup key (_bindings ctx) of
    Nothing -> "uninitialised"
    Just v  -> show v

instance Show a => Show (Context a) where
  show ctx = unlines [ "Path constraint: " <> show (_pathCondition ctx)
                     , showKey ctx IR
                     ]
