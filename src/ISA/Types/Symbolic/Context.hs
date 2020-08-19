module ISA.Types.Symbolic.Context
  (Context(..), showKey, showIR) where

import qualified Data.Map.Strict              as Map

import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic

-- | A record type for state of the symbolically executed computation
--   * @_bindings@: keys (like register names, memory cells) mapped to their symbolic values
--   * @_pathCondition@ : a symbolic expression which must hold for this state to be
--     reachable
--   * @_fmapLog@: a stack of values of @fmap@'s second arguments.
--     Consider @fmap (f :: a -> b) (x :: Engine a)@, then after executing this
--     computations: @head (_fmapLog s)@ will contain the "purified" @x@, i.e.
--     a value of type @a@ coerced to 'GHC.Exts.Any'. This field is modified
--     via 'pushFmapArg' and 'popFmapArg' functions.
--     (TODO: come up with a better explanation for this)
data Context = MkContext { _bindings      :: Map.Map Key Sym
                         , _pathCondition :: Sym
                         }

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
                     ]
