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
    ( gatherFree
    , sat
    ) where

import qualified Data.Map         as Map
import qualified Data.SBV.Dynamic as SBV
import qualified Data.Set         as Set
import           Data.Text        (Text)
import qualified Data.Text        as Text

import           ToyRISC.Symbolic


-- | Walk the symbolic expression gathering up the free variables.
gatherFree :: SExpr -> Set.Set SExpr
gatherFree = \case
  Concrete _  -> mempty
  var@(Any _) -> Set.singleton var
  Symbolic op args -> case op of
    Plus  -> gatherFree (_1 args) <> gatherFree (_2 args)
    Times -> gatherFree (_1 args) <> gatherFree (_2 args)
    Minus -> gatherFree (_1 args) <> gatherFree (_2 args)
    Neg   -> gatherFree (_1 args)
    Abs   -> gatherFree (_1 args)
    Not   -> gatherFree (_1 args)
    And   -> gatherFree (_1 args) <> gatherFree (_2 args)
    Or    -> gatherFree (_1 args) <> gatherFree (_2 args)
    Eq    -> gatherFree (_1 args) <> gatherFree (_2 args)
    Lt    -> gatherFree (_1 args) <> gatherFree (_2 args)
    Gt    -> gatherFree (_1 args) <> gatherFree (_2 args)
  where _1 :: [a] -> a
        _1 = head

        _2 :: [a] -> a
        _2 = head . tail

-- | Create existential variables for each of Any's in the input.
createSym :: [SExpr] -> SBV.Symbolic (Map.Map Text SBV.SVal)
createSym cs = do
  pairs <- traverse createSymPair cs
  pure $ Map.fromList pairs
    where createSymPair :: SExpr -> SBV.Symbolic (Text, SBV.SVal)
          createSymPair (Any name) = do
            v <- SBV.sIntN 32 (Text.unpack name)
            pure (name, v)
          createSymPair _ = error "Non-variable encountered."

-- | Translate symbolic values into the SBV representation
symToSMT :: Map.Map Text SBV.SVal -> SExpr -> SBV.Symbolic (SBV.SVal)
symToSMT env = \case
  Concrete c -> case c of
                  (CBounded i) ->
                    pure (SBV.svInteger (SBV.KBounded True 32) (fromIntegral i))
                  (CChar _)    -> error "SMT.symToSMT: Char support is not implemented"
                  (CBool b)    -> pure (SBV.svBool b)
  Any   name -> case Map.lookup name env of
                  Just val -> pure val
                  Nothing  -> error $ "SMT.symToSMT: Missing symbolic variable with name " <>
                      (Text.unpack name)
  Symbolic op args ->
    case op of
      Plus  -> (SBV.svPlus)  <$> symToSMT env (_1 args) <*> symToSMT env (_2 args)
      Times -> (SBV.svTimes) <$> symToSMT env (_1 args) <*> symToSMT env (_2 args)
      Minus -> (SBV.svMinus) <$> symToSMT env (_1 args) <*> symToSMT env (_2 args)
      Neg   -> (SBV.svUNeg)  <$> symToSMT env (_1 args)
      Abs   -> (SBV.svAbs)   <$> symToSMT env (_1 args)
      Not   -> (SBV.svNot)   <$> symToSMT env (_1 args)
      And   -> (SBV.svAnd)   <$> symToSMT env (_1 args) <*> symToSMT env (_2 args)
      Or    -> (SBV.svOr)    <$> symToSMT env (_1 args) <*> symToSMT env (_2 args)
      Eq    -> (SBV.svEqual)          <$> symToSMT env (_1 args) <*> symToSMT env (_2 args)
      Lt    -> (SBV.svLessThan)       <$> symToSMT env (_1 args) <*> symToSMT env (_2 args)
      Gt    -> (SBV.svGreaterThan)    <$> symToSMT env (_1 args) <*> symToSMT env (_2 args)
  where _1 :: [a] -> a
        _1 = head

        _2 :: [a] -> a
        _2 = head . tail

-- | Convert a list of (boolean) symbolic expressions
--   to a symbolic value the SMT solver can solve.
--   Each constraint in the list is conjoined with the others.
toSMT :: [SExpr] -> SBV.Symbolic SBV.SVal
toSMT cs = do
  let freeVars = gatherFree (foldr (\x y -> Symbolic And [x, y]) sTrue cs)
  sValMap <- createSym (Set.toList freeVars)
  smts <- traverse (symToSMT sValMap) cs
  pure $ conjoin smts
  where
    conjoin :: [SBV.SVal] -> SBV.SVal
    conjoin xs = foldr SBV.svAnd SBV.svTrue xs

-- | Check satisfiability of an expression
sat :: SExpr -> IO SBV.SatResult
sat expr = do
  let smtExpr = toSMT [expr]
  SBV.satWith solver smtExpr

-----------------------------------------------------------------------------
-- | SMT solver configurations
solver :: SBV.SMTConfig
solver = SBV.z3 { SBV.verbose = True
                , SBV.redirectVerbose = Just "./smt-logs/log.smt2"
                , SBV.printBase = 16
                }
