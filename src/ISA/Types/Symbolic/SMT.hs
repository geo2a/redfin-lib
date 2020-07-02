-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Symbolic.SMT
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Semantics of symbolic expressions in terms of SMT
--
-----------------------------------------------------------------------------

module ISA.Types.Symbolic.SMT
    ( -- get the list of free variables in a symbolic expression
      gatherFree
      -- check if the expression is satisfiable
    , sat, Options (..), solve
      -- helper functions
    , conjoin, isSat, isUnsat
    ) where

import qualified Data.Map.Strict    as Map
import qualified Data.SBV.Dynamic   as SBV
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           System.IO.Unsafe   (unsafePerformIO)

import           ISA.Types.Symbolic

-- | Walk the constraint gathering up the free variables.
gatherFree :: Sym -> Set.Set Sym
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
createSym :: [Sym] -> SBV.Symbolic (Map.Map Text SBV.SVal)
createSym cs = do
  pairs <- traverse createSymPair cs
  pure $ Map.fromList pairs
    where createSymPair :: Sym -> SBV.Symbolic (Text, SBV.SVal)
          createSymPair (SAny name) = do
            v <- SBV.sIntN 32 (Text.unpack name)
            pure (name, v)
          createSymPair _ = error "Non-variable encountered."

-- | Convert a list of path constraints to a symbolic value the SMT solver can solve.
--   Each constraint in the list is conjoined with the others.
toSMT :: [Sym] -> SBV.Symbolic SBV.SVal
toSMT cs = do
  let freeVars = gatherFree (foldr SAnd (SConst (CBool True)) cs)
  sValMap <- createSym (Set.toList freeVars)
  smts <- traverse (symToSMT sValMap) cs
  pure $ conjoinSBV smts

-- | Translate symbolic values into the SBV representation
symToSMT :: Map.Map Text SBV.SVal -> Sym -> SBV.Symbolic SBV.SVal
symToSMT m (SEq l r) =
  (SBV.svEqual) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SGt l r) =
  (SBV.svGreaterThan) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SLt l r) =
  (SBV.svLessThan) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SAdd l r) =
  (SBV.svPlus) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SSub l r) =
  (SBV.svMinus) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SMul l r) =
  (SBV.svTimes) <$> symToSMT m l <*> symToSMT m r
symToSMT _ (SDiv _ _) = error "SMT.symToSMT: div is not yet defined"
--   (SBV.svDiv) <$> symToSMT m l <*> symToSMT m r
symToSMT _ (SMod _ _) = error "SMT.symToSMT: mod is not yet defined"
--   (SBV.svMod) <$> symToSMT m l <*> symToSMT m r
symToSMT _ (SConst (CWord w)) = pure (SBV.svInteger (SBV.KBounded False 16) (fromIntegral w))
symToSMT _ (SConst (CInt32 i)) = pure (SBV.svInteger (SBV.KBounded True 32) (fromIntegral i))
symToSMT _ (SConst (CBool b)) = pure (SBV.svBool b)
symToSMT m (SAbs l) =
  SBV.svAbs <$> symToSMT m l
symToSMT m (SNot c) =
  SBV.svNot <$> symToSMT m c
symToSMT m (SAnd l r) =
  SBV.svAnd <$> symToSMT m l <*> symToSMT m r
symToSMT m (SOr l r) =
  SBV.svOr <$> symToSMT m l <*> symToSMT m r
symToSMT m (SAny i) =
  case Map.lookup i m of
    Just val -> pure val
    Nothing  -> error "Missing symbolic variable."

-- | Check satisfiability of a boolean expression
sat :: Sym -> IO SBV.SatResult
sat expr = do
  let smtExpr = toSMT [expr]
  SBV.satWith solver smtExpr

-- -- -- | Solve the path constraints in a symbolic execution state
-- -- solveSym :: State -> IO SolvedState
-- -- solveSym state = do
-- --     let smtExpr = toSMT . map snd $ pathConstraintList state
-- --     SBV.SatResult smtRes <- SBV.satWith prover smtExpr
-- --     pure (SolvedState state smtRes)

-- -- -- | Traverse a symbolic execution trace and solve path constraints in every node
-- -- solveTrace :: Trace State -> IO (Trace SolvedState)
-- -- solveTrace = traverse solveSym

conjoinSBV :: [SBV.SVal] -> SBV.SVal
conjoinSBV = foldr (\x y -> SBV.svAnd x y) (SBV.svBool True)

conjoin :: [Sym] -> Sym
conjoin cs = foldr (\x y -> SAnd x y) (SConst (CBool True)) cs

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
             | Sat Sym
             -- ^ the path constraint is a satisfiable symbolic boolean -- need to create
             --   two worlds: for it and its negation

solve :: Sym -> Int -> Options
solve expr fuel =
  case getValue (simplify (Just fuel) expr) of
    Just (CBool val) -> Literal val
    Just (CWord w) -> error $ "Sym.solve: non-boolean literal " <> show w
    Just (CInt32 i) -> error $ "Sym.solve: non-boolean literal " <> show i
    Nothing ->
      let (SBV.SatResult result) = unsafePerformIO (sat expr)
      in case result of
           SBV.Unsatisfiable _ _ -> DeadEnd
           SBV.Satisfiable _ _   -> Sat (expr)
           _                     -> error "Sym.solveBoolExpr: fatal error!"
