{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.SMT
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- SMT bindings via SBV
--
-----------------------------------------------------------------------------

module ToyRISC.SMT
    ( -- get the list of free variables in a symbolic expression
      gatherFree
      -- check if the expression is satisfiable
    , sat, Options (..), solve
      -- helper functions
    , conjoin, isSat, isUnsat
    ) where

import           Data.Int         (Int32)
import qualified Data.Map.Strict  as Map
import qualified Data.SBV         as SBV
import qualified Data.Set         as Set
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           System.IO.Unsafe (unsafePerformIO)

import           ToyRISC.Symbolic

-- | Walk the constraint gathering up the free variables.
gatherFree :: Sym a -> Set.Set (Sym Int32)
gatherFree c@(SAny _) = Set.singleton c
gatherFree (SAdd l r) = gatherFree l <> gatherFree r
gatherFree (SSub l r) = gatherFree l <> gatherFree r
gatherFree (SMul l r) = gatherFree l <> gatherFree r
gatherFree (SDiv l r) = gatherFree l <> gatherFree r
gatherFree (SMod l r) = gatherFree l <> gatherFree r
gatherFree (SAbs l)   = gatherFree l
gatherFree (SNot c)   = gatherFree c
gatherFree (SOr l r)  = gatherFree l <> gatherFree r
gatherFree (SAnd l r) = gatherFree l <> gatherFree r
gatherFree (SEq l r)  = gatherFree l <> gatherFree r
gatherFree (SGt l r)  = gatherFree l <> gatherFree r
gatherFree (SLt l r)  = gatherFree l <> gatherFree r
gatherFree (SConst _) = mempty

-- | Create existential SVals for each of SAny's in the input.
createSym :: [Sym Int32] -> SBV.Symbolic (Map.Map Text SBV.SInt32)
createSym cs = do
  pairs <- traverse createSymPair cs
  pure $ Map.fromList pairs
    where createSymPair :: Sym Int32 -> SBV.Symbolic (Text, SBV.SInt32)
          createSymPair (SAny i) = do
            v <- SBV.sInt32 (Text.unpack i)
            pure (i, v)
          createSymPair _ = error "Non-variable encountered."

-- | Convert a list of path constraints to a symbolic value the SMT solver can solve.
--   Each constraint in the list is conjoined with the others.
toSMT :: [Sym Bool] -> SBV.Symbolic SBV.SBool
toSMT cs = do
  let freeVars = gatherFree (foldr SAnd (SConst True) cs)
  sValMap <- createSym (Set.toList freeVars)
  smts <- traverse (symToSMT sValMap) cs
  pure $ conjoinSBV smts

-- | Translate type indices of the Sym GADT into SBV phantom types
type family ToSBV a where
    ToSBV Int32 = SBV.SBV Int32
    ToSBV Bool  = SBV.SBV Bool
    ToSBV a     = SBV.SBV a

-- | Translate symbolic values into the SBV representation
symToSMT :: SBV.SymVal a => Map.Map Text SBV.SInt32 -> Sym a -> SBV.Symbolic (ToSBV a)
symToSMT m (SEq l r) =
  (SBV..==) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SGt l r) =
  (SBV..>) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SLt l r) =
  (SBV..<) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SAdd l r) =
  (+) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SSub l r) =
  (-) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SMul l r) =
  (*) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SDiv l r) =
  SBV.sDiv <$> symToSMT m l <*> symToSMT m r
symToSMT m (SMod l r) =
  SBV.sMod <$> symToSMT m l <*> symToSMT m r
symToSMT _ (SConst w) = pure (SBV.literal w)
symToSMT m (SAbs l) =
  abs <$> symToSMT m l
symToSMT m (SNot c) =
  SBV.sNot <$> symToSMT m c
symToSMT m (SAnd l r) =
  (SBV..&&) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SOr l r) =
  (SBV..||) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SAny i) =
  case Map.lookup i m of
    Just val -> pure val
    Nothing  -> error "Missing symbolic variable."

-- | Check satisfiability of a boolean expression
sat :: Sym Bool  -> IO SBV.SatResult
sat expr = do
  let smtExpr = toSMT [expr]
  SBV.satWith solver smtExpr

-- -- | Solve the path constraints in a symbolic execution state
-- solveSym :: State -> IO SolvedState
-- solveSym state = do
--     let smtExpr = toSMT . map snd $ pathConstraintList state
--     SBV.SatResult smtRes <- SBV.satWith prover smtExpr
--     pure (SolvedState state smtRes)

-- -- | Traverse a symbolic execution trace and solve path constraints in every node
-- solveTrace :: Trace State -> IO (Trace SolvedState)
-- solveTrace = traverse solveSym

conjoinSBV :: [SBV.SBool] -> SBV.SBool
conjoinSBV = SBV.sAnd

conjoin :: [Sym Bool] -> Sym Bool
conjoin cs = foldr (\x y -> SAnd x y) (SConst True) cs

solver :: SBV.SMTConfig
solver = SBV.z3 { SBV.verbose = True
                , SBV.redirectVerbose = Just "log.smt2"
                , SBV.printBase = 16
                }

isSat :: SBV.SatResult -> Bool
isSat (SBV.SatResult r) = case r of
  (SBV.Satisfiable _ _) -> True
  _                     -> False

isUnsat :: SBV.SatResult -> Bool
isUnsat (SBV.SatResult r) = case r of
  (SBV.Unsatisfiable _ _) -> True
  _                       -> False
-----------------------------------------------------------------------------
data Options = DeadEnd
             -- ^ the path constraint is unsatisfiable
             | Literal Bool
             -- ^ the path constraint is a literal boolean value, continue in this world
             | Sat (Sym Bool)
             -- ^ the path constraint is a satisfiable symbolic boolean -- need to create
             --   two worlds: for it and its negation

solve :: Int -> Sym Bool -> Options
solve fuel expr =
  case (getValue . simplify fuel $ expr) of
    Just val -> Literal val
    Nothing ->
      let (SBV.SatResult result) = unsafePerformIO (sat expr)
      in case result of
           SBV.Unsatisfiable _ _ -> DeadEnd
           SBV.Satisfiable _ _   -> Sat (expr)
           _                     -> error "Sym.solveBoolExpr: fatal error!"
