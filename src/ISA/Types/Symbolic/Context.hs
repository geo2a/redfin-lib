{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ISA.Types.Symbolic.Context
  ( Context(..), emptyCtx
  , getBinding, showKey, showIR, isReachable) where

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

-- | A record type for state of the symbolically executed computation
--   * @_bindings@: keys (like register names, memory cells) mapped to their symbolic values
--   * @_pathCondition@ : a symbolic expression which must hold for this state to be
--     reachable
data Context = MkContext { _bindings      :: Map.Map Key Sym
                         , _pathCondition :: Sym
                         , _constraints   :: [(Text, Sym)]
                         , _solution      :: Maybe SMTResult
                         }
  deriving (Generic, ToJSON, FromJSON)

emptyCtx :: Context
emptyCtx = MkContext Map.empty false [] Nothing

instance Eq Context where
  x == y = (_bindings x == _bindings y)
        && (_constraints x == _constraints y)

isReachable :: Context -> Bool
isReachable ctx = case (_solution ctx) of
  Nothing              -> True
  Just (Satisfiable _) -> True
  _                    -> False

getBinding :: Key -> Context -> Maybe Sym
getBinding key ctx = Map.lookup key (_bindings ctx)

showKey :: Context -> Key -> String
showKey ctx key =
  case Map.lookup key (_bindings ctx) of
    Nothing -> "uninitialised"
    Just v  ->
      if key == IR
      then show key <> ": " <> show (toInstruction v)
      else show key <> ": " <> show v

showIR :: Context -> String
showIR ctx =
  case Map.lookup IR (_bindings ctx) of
    Nothing -> "uninitialised"
    Just v  ->
      case toInstruction v of
        Left _  -> "uninitialised"
        Right i -> show i

instance Show Context where
  show ctx = unlines [ "Path constraint: " <> show (_pathCondition ctx)
                     , showKey ctx IR
                     , showKey ctx (F Condition)
                     , showKey ctx (F Halted)
                     , showKey ctx (F Overflow)
                     ]
