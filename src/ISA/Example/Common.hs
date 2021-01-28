module ISA.Example.Common where

import           Control.Monad.State.Strict
import           Data.Bifunctor             (first)
import qualified Data.Map.Strict            as Map
import           Data.SBV                   (SMTConfig (..), Timing (..), z3)
import           Data.Text                  (Text)

import           ISA.Assembly
import           ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace

defaultRegisters :: [(Register, Sym)]
defaultRegisters = [ (R0, 0)
                   , (R1, 1)
                   , (R2, 2)
                   , (R3, 3)
                   ]

defaultFlags :: [(Flag, Sym)]
defaultFlags = [ (Halted, SConst (CBool False))
               , (Overflow, SConst (CBool False))
               , (Condition, SConst (CBool False))
               , (Zero, SConst (CBool False))
               ]

mkMemory :: [(Address, Sym)] -> [(Address, Sym)]
mkMemory = id

boot :: Script
     -> [(Register, Sym)]
     -> [(Address,  Sym)]
     -> [(Flag, Sym)]
     -> Symbolic Context
boot src regs memory flags = do
  cs <- _constraints <$> get
  pure $ MkContext { _pathCondition = SConst (CBool True)
                   , _constraints = cs
                   , _bindings =
                       Map.fromList $ mkProgram src
                                   ++ map (first Reg) regs
                                   ++ map (first Addr) memory
                                   ++ map (first F) flags
                                   ++ [(IC, SConst 0), (IR, 0)]
                   }

newtype Symbolic a = MkSymbolic { getSymbolic :: StateT Context IO a }
  deriving (Functor, Applicative, Monad, MonadState Context, MonadIO)

runSymbolic :: Symbolic a -> IO (a, Context)
runSymbolic theorem =
  runStateT (getSymbolic theorem) (MkContext Map.empty (SConst (CBool True)) [] Nothing)

forall :: Text -> Symbolic Sym
forall = pure . SAny

-- | Generate free variables with the given names
symbolics :: [Text] -> Symbolic [Sym]
symbolics names = pure $ map SAny names

-- | Conjunct a symbolic constraint to the current path condition
constrain :: (Text, Context -> Sym) -> Symbolic ()
constrain (name, expr) = modify (\ctx -> ctx {_constraints = (name, expr ctx):_constraints ctx})

prover :: SMTConfig
prover = z3 { verbose = True
            , redirectVerbose = Just "example.smt2"
            , timing = PrintTiming
            , printBase = 10
            }