{-# LANGUAGE DeriveAnyClass #-}

module ISA.Types.CTL.Model where
-- module ISA.Types.CTL.Model
--   ( Theorem(..), SolverTask(..), Constraints(..), Proof(..), atomicTasks
--   , formulate, prove
--   , showCTL) where


import           Control.Concurrent.STM          hiding (check)
import           Control.Concurrent.STM.TQueue
import           Control.Monad                   (join)
import           Control.Monad.IO.Class
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Bifunctor
import           Data.List                       (partition)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (isJust)
import           Data.Monoid
import qualified Data.SBV                        as SBV
import qualified Data.SBV.Control                as SBV
import qualified Data.SBV.Internals              as SBV
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Traversable
import           Data.Tree                       (Tree)
import qualified Data.Tree                       as Tree
import qualified Debug.Trace                     as Bug
import           GHC.Generics
import           Numeric.Natural

import           ISA.Backend.Symbolic.List.Trace
import           ISA.Types
import           ISA.Types.CTL
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT

newtype Variable = MkVar Text
  deriving (Show, Generic, ToJSON, FromJSON)

-- | A theorem is a model for a CTL formula in terms of a symbolic expressions.
newtype Theorem = MkTheorem { getTheorem :: (SolverTask Sym) }
  deriving (Generic, ToJSON, FromJSON)

-- | An operation to combine the results of solving: either conjunction or disjunction
data Op = Conj | Disj
  deriving (Generic, ToJSON, FromJSON)

op :: Op -> ([Sym] -> Sym)
op = \case Conj -> conjoin
           Disj -> disjoin

-- | A task prepared for solving:
--   * either an atomic symbolic expression coming
--     from a certain node in the trace
--   * or a conjunction/disjunction of tasks
data SolverTask a = Atomic (NodeId, a)
                  | Compound Op [SolverTask a]
  deriving (Functor, Generic, ToJSON, FromJSON)

instance Foldable SolverTask where
  foldMap f  = \case
    Atomic (_, x) -> f x
    Compound _ tasks -> mconcat (map (foldMap f) tasks)

instance Traversable SolverTask where
  traverse action = \case
    Atomic (n , x) -> Atomic . (n,) <$> action x
    Compound op tasks -> Compound op <$> traverse (traverse action) tasks

-- | The amount of atomic tasks in a solver task tree
size :: SolverTask a -> Int
size = getSum . foldMap (const (Sum 1))

-- | Depth of solver task tree
depth :: SolverTask a -> Natural
depth = \case Atomic _ -> 0
              Compound _ xs -> sum . map depth $ xs

-- | Get a list of atomic solver tasks: useful for gathering free variables
atomicTasks :: SolverTask a -> [(NodeId, a)]
atomicTasks = go []
  where go acc = \case
          Atomic n -> n : acc
          Compound _ xs -> concatMap (go acc) xs

instance Show a => Show (SolverTask a) where
  show = \case
    Atomic (n, s) -> "In node " <> show n <> ": " <> show s
    Compound op xs -> case op of
      Conj -> unlines (map (("∧ " <>) . show) xs)
      Disj -> unlines (map (("∨ " <>) . show) xs)

showCTL :: CTL (Context -> Sym) -> Context -> String
showCTL prop c = case prop of
  TT      -> "T"
  FF      -> "⊥"
  Atom p  -> show (p c)
  Not p   -> "¬" <> showCTL p c
  And p q -> showCTL p c <> " ∧ " <> showCTL q c
  AllG p  -> "AG " <> showCTL p c
  AllF p  -> "AF" <> showCTL p c

-- | Result of theorem proving proving
data Proof = Proved
           | Falsifiable [(NodeId, SMTResult)]
           deriving (Generic, ToJSON, FromJSON)

instance Show Proof where
  show = \case
    Proved -> "Q.E.D"
    Falsifiable contra ->
      "Falsifiable! Counterexample caused by nodes " <> (unlines $ map show contra)


-- | A list of symbolic expressions to serve as constraints for the state space
newtype Constraints = ConstrainedBy [Sym]
  deriving (Show, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
data Schedule a = Literal a
                | Conjunct [Schedule a]
                | Disjunct [Schedule a]
                deriving (Show, Functor)

flatten :: Schedule a -> [a]
flatten = go []
  where
    go acc = \case
      Literal x -> x : acc
      Conjunct xs -> concatMap flatten xs
      Disjunct xs -> concatMap flatten xs

-- instance Show a => Show (Schedule a) where
--   show = \case
--     Literal l -> show l
--     Conjunct xs -> "(" <> (concatMap (\x -> show x <> " ∧ ") xs) <> ")"
--       -- unlines (map (("∧ " <>) . show) xs)
--     Disjunct xs -> "(" <> (concatMap (\x -> show x <> " ∨ ") xs) <> ")"

deMorgan :: Schedule (NodeId, Sym) -> Schedule (NodeId, Sym)
deMorgan = \case
  Literal (n, v) -> Literal (n, SNot v)
  Conjunct xs -> Disjunct (map deMorgan xs)
  Disjunct xs -> Conjunct (map deMorgan xs)


-- | Interpret a CTL formula as a collection of Sym's, preparing it for solving
formulate :: CTL (Context -> Sym) -> Trace Context -> Schedule (NodeId, Sym)
formulate property trace =
  let initId = _nodeId . initial $ trace
  in case property of
       TT      -> Literal (initId , true)
       FF      -> Literal (initId , false)
       Atom p  -> Literal (initId , (p (_nodeBody $ initial trace)))
       Not p   -> deMorgan (formulate p trace)
       -- Not p   -> second SNot <$> (formulate p trace)
       And p q -> Conjunct [formulate p trace, formulate q trace]
       AllG p   -> Conjunct . map (\node -> formulate p (mkTrace node []))
                   . nodes $ trace
       EG p -> Disjunct . map (\node -> formulate p (mkTrace node []))
             . nodes $ trace
       AllF p -> formulate (Not (EG (Not p))) trace
       -- AllF p -> allFImpl p (unTrace trace)
  -- where
  --   allFImpl :: CTL (Context -> Sym)
  --            -> Tree (Node Context) -> Schedule (NodeId, Sym)
  --   allFImpl p (Tree.Node x xs) =
  --     case xs of
  --       [] -> formulate p (mkTrace x [])
  --       -- (y:[]) -> Disjunct [ formulate p (mkTrace x []), map (allFImpl p) y]
  --       (ys) -> Disjunct [ formulate p (mkTrace x [])
  --                        , Conjunct (map (allFImpl p) ys)
  --                        ]

postprocess :: Schedule (NodeId, SMTResult) -> [(NodeId, SMTResult)]
postprocess schedule =
  let sats = postprocessImpl [] schedule
  in case schedule of
       Literal _  -> sats
       Conjunct _ -> if all (isSat . snd) sats then sats else []
       Disjunct _ -> if any (isSat . snd) sats then sats else []

postprocessImpl :: [(NodeId, SMTResult)] -> Schedule (NodeId, SMTResult) -> [(NodeId, SMTResult)]
postprocessImpl acc = \case
  Literal x -> if (isSat . snd) x then x : acc else acc
  Conjunct xs ->
    let results = concatMap (postprocessImpl acc) xs
    in case all (isSat . snd) results of
         True  -> acc
         False -> []
  Disjunct xs ->
    let results = concatMap (postprocessImpl acc) xs
    in case any (isSat . snd) results of
         True  -> acc
         False -> []

consume :: Schedule (NodeId, TMVar SMTResult) -> STM (Schedule (NodeId, SMTResult))
consume schedule = case schedule of
  Literal (nid, var) -> Literal . (nid,) <$> takeTMVar var
  Conjunct xs        -> Conjunct <$> mapM consume xs
  Disjunct xs        -> Disjunct <$> mapM consume xs

produce :: TVar (Map Text SBV.SInt32)
        -> Schedule (NodeId, Sym) -> IO ([SBV.Query ()], Schedule (NodeId, TMVar SMTResult))
produce vars schedule =
  (\r -> (flatten (fmap (\(_, q, _) -> q) r), fmap (\(n, _, v) -> (n, v)) r)) <$> go schedule
  where
    go schedule =
      case schedule of
        Literal (nid, sym) -> do
          resultBox <- newEmptyTMVarIO
          let query = nodeQuery vars resultBox sym
          pure (Literal (nid, query, resultBox))
        Conjunct xs        -> Conjunct <$> mapM go xs
        Disjunct xs        -> Disjunct <$> mapM go xs

-- | Prepare the shared proving environment by communicating free variables to SBV
prepare :: TVar (Map Text SBV.SInt32) -> Set Sym -> Constraints -> SBV.Symbolic ()
prepare env freeVars (ConstrainedBy cs) = do
  vars <- createSym $ Set.toList freeVars
  liftIO . atomically $ writeTVar env vars
  pre <- toSMT vars cs
  SBV.constrain pre

-- | Check if a property is satisfiable on the given trace under the given constraints
--   Return an empty list if all nodes are unsats and the satisfiable nodes otherwise
sat :: CTL (Context -> Sym) -> Trace Context -> Constraints -> IO [(NodeId, SMTResult)]
sat prop trace cs = do
  let schedule = formulate prop trace
      freeVars = gatherFree . conjoin . map snd . flatten $ schedule
  env <- newTVarIO mempty
  (qs, sch) <- produce env schedule
  _ <- SBV.satConcurrentWithAll SBV.z3 qs (prepare env freeVars cs)
  solutions <- atomically (consume sch)
  pure (postprocess solutions)

-- | An SMT-problem constructed from a node
nodeQuery :: TVar (Map Text SBV.SInt32) -> TMVar SMTResult -> Sym -> SBV.Query ()
nodeQuery env resultBox expr = do
  vars <- liftIO $ readTVarIO env
  SBV.constrain =<< toSMT vars [expr]
  SBV.checkSat >>= \case
    SBV.Unk ->
      error "Impossible happened: prover said unknown!"
    _ -> SBV.getSMTResult >>= \case
      (SBV.Satisfiable _ _) -> do
        values <- traverse SBV.getValue vars
        liftIO . atomically $ do
          putTMVar resultBox $ Satisfiable . MkSMTModel $ values
      (SBV.Unsatisfiable _ _) -> do
        liftIO . atomically $ do
          putTMVar resultBox $ Unsatisfiable
      _ -> error "not implemented"

-- | Prove a CTL property on a trace
--   A property is valid if its negation is unsatisfiable, i.e.
prove :: CTL (Context -> Sym) -> Trace Context -> Constraints ->IO Proof
prove property trace precondition =
  sat (Not property) trace precondition >>= \case
    [] -> pure Proved
    contra -> pure $ Falsifiable contra
