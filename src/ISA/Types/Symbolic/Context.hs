{-# LANGUAGE DeriveGeneric #-}

module ISA.Types.Symbolic.Context
  (Context(..), getBinding, showKey, showIR, isReachable) where

import qualified Data.Map.Strict              as Map
import qualified Data.SBV                     as SBV (SMTResult (..),
                                                      SatResult (..))
import           Data.Text                    (Text)
import           GHC.Generics

import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic

-- | A record type for state of the symbolically executed computation
--   * @_bindings@: keys (like register names, memory cells) mapped to their symbolic values
--   * @_pathCondition@ : a symbolic expression which must hold for this state to be
--     reachable
data Context = MkContext { _bindings      :: Map.Map Key Sym
                         , _pathCondition :: Sym
                         , _constraints   :: [(Text, Sym)]
                         , _solution      :: Maybe SBV.SatResult
                         }
  deriving (Generic)

instance Eq Context where
  x == y = _bindings x == _bindings y

isReachable :: Context -> Bool
isReachable ctx = case (_solution ctx) of
  Nothing                                    -> True
  Just (SBV.SatResult (SBV.Satisfiable _ _)) -> True
  _                                          -> False

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
