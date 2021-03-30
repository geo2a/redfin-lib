-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.ACTL.Model
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Semantics for 'ISA.Types.Symbolic.ACTL.ACTL'
--
-----------------------------------------------------------------------------

module ISA.Types.Symbolic.ACTL.Model
  where

import           Data.Bifunctor
import qualified Data.IntMap                        as IntMap
import qualified Data.IntSet                        as IntSet
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set

import           ISA.Backend.Symbolic.Zipper
import           ISA.Types.Context
import           ISA.Types.Prop
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.ACTL
import           ISA.Types.Symbolic.Address
import           ISA.Types.Symbolic.SMT.Problem
import           ISA.Types.Symbolic.SMT.Solving
import           ISA.Types.Symbolic.SMT.Translation
import           ISA.Types.Tree

-- | Interpret an ACTL formula on a trace, yielding an SMT problem
evalACTL :: Trace -> ACTL -> Problem Sym ([(Address, Sym)], Sym)
evalACTL trace prop = do
  let resolved = trace -- resolvePointers trace
  let symVars = Set.toList  . mconcat . map findFreeVars . IntMap.elems $ _states resolved
  MkProblem symVars (go resolved prop)
  where
    go trace = \case
      ACTLAllG atom ->
        tasks .
        map (task . translateState atom) .
        IntMap.assocs $ _states trace
      ACTLAllF atom ->
        let leafIds = IntSet.fromList (leafs (_layout trace))
            leafStates = IntMap.restrictKeys (_states trace) leafIds
        in tasks .
           map (task . translateState atom) .
           IntMap.assocs $ leafStates
      ACTLAnd p q -> tasks [go trace p, go trace q]

-- negateProblem :: Problem Sym ([(Address, Sym)], Sym) -> Problem Sym ([(Address, Sym)], Sym)
-- negateProblem p = p {_task = fmap (second SNot) (_task p)}

-- | Interpret an atomic formula at the given state,
--   considering the state's symbolic memory and path condition
translateState :: Atom -> (Int, Context Sym)
               -> Atomic ([(Address, Sym)], Sym)
translateState prop (n, ctx) =
  let memory = dumpMemory ctx
      eqs = foldr (&&&) true
          . map (\(name, expr) -> SAny name === expr)
          . Map.assocs $ _store ctx
      cs = foldr (&&&) true
         . map snd
         . _constraints $ ctx
      exprs = _pathCondition ctx &&& eqs &&& cs &&& evalAtom prop ctx
  in MkAtomic (n, (memory, exprs))

-- | A 'Proof' is the final result of proving.
--  It is either proved or provides a counter example path
data Proof = Proved ACTL
           | Falsifiable ACTL [(Int, SMTResult)]

instance Show Proof where
  show (Proved _) = "Q.E.D."
  show (Falsifiable _ contra) =
    "Falsifiable! Counterexample states: " <> show (map fst contra)

prove :: Trace -> ACTL -> IO Proof
prove trace prop = do
  sat (evalACTL trace $ negateACTL prop) >>= \xs ->
    case filter (isSat . snd) $ IntMap.toList xs  of
      []     -> pure $ Proved prop
      contra -> pure $ Falsifiable prop contra
