{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module ISA.Types.CTL.Model
  ( Theorem(..), SolverTask(..)
  , formulate, Constraints(..), prove) where

-- module ISA.Types.CTL.Model where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad.IO.Class
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Monoid
import qualified Data.SBV                      as SBV
import qualified Data.SBV.Control              as SBV
import qualified Data.SBV.Internals            as SBV
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Traversable
import           Data.Tree                     (Tree)
import qualified Data.Tree                     as Tree
import           GHC.Generics
import           Numeric.Natural

import           ISA.Types
import           ISA.Types.CTL
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Symbolic.Trace

newtype Variable = MkVar Text
  deriving (Show, Generic, ToJSON, FromJSON)

-- | A theorem is a model for a CTL formula in terms of a symbolic expressions.
newtype Theorem = MkTheorem { getTheorem :: (SolverTask Sym) }
  deriving (Generic, ToJSON, FromJSON)

data Op = Conj | Disj
  deriving (Generic, ToJSON, FromJSON)

op :: Op -> ([Sym] -> Sym)
op = \case Conj -> conjoin
           Disj -> disjoin

-- | A task prepared for  solving:
--   * either an atomic symbolic expression coming
--     from a certain node in the trace
--   * or a conjunction/disjunction of tasks
data SolverTask a = Atomic (Text, a)
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

size :: SolverTask a -> Int
size = getSum . foldMap (const (Sum 1))

depth :: SolverTask a -> Natural
depth = \case Atomic _ -> 0
              Compound _ xs -> sum . map depth $ xs

-- | Get a list of atomic solver tasks: useful for gathering free variables
atomicTasks :: SolverTask a -> [(Text, a)]
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

showCTL :: Context -> CTL (Context -> Sym) -> String
showCTL init = \case
  TT      -> "T"
  FF      -> "⊥"
  Atom p  -> show (p init)
  Not p   -> "¬" <> showCTL init p
  And p q -> showCTL init p <> " ∧ " <> showCTL init q
  AllG p  -> "AG " <> showCTL init p
  AllF p  -> "AF" <> showCTL init p

-- instance Show Theorem where
--   show (MkTheorem exprs) = unlines $
--     map (show . _nodeBody) . Set.toList $ exprs

-- -- | Result of @Theorem proving
-- data Proof = Proved Theorem
--            | Falsifiable Theorem (NodeId, SMTResult)
--            deriving (Generic, ToJSON, FromJSON)

-- instance Show Proof where
--   show = \case
--     Proved _ -> "Q.E.D"
--     Falsifiable theorem (n, contra) ->
--       "Falsifiable! Counterexample caused by node " <> show n <> ": \n" <> show contra

formulate :: CTL (Context -> Sym) -> Trace Context -> Theorem
formulate p t = MkTheorem (formulateImpl p t)

formulateImpl :: CTL (Context -> Sym) -> Trace Context -> SolverTask Sym
formulateImpl property trace =
  let initId = _nodeId . initial $ trace
  in case property of
     TT      -> Atomic $ (Text.pack . show $ initId , true)
     FF      -> Atomic $ (Text.pack . show $ initId , false)
     Atom p  -> Atomic $ (Text.pack . show $ initId , (p (_nodeBody $ initial trace)))
     Not p   -> SNot <$> formulateImpl p trace
     And p q -> Compound Conj [formulateImpl p trace, formulateImpl q trace]
     AllG p   -> Compound Conj . map (\node -> formulateImpl p (mkTrace node []))
               . nodes $ trace
     EG p -> Compound Disj . map (\node -> formulateImpl p (mkTrace node []))
             . nodes $ trace
     AllF p -> allFImpl p (unTrace trace)
  where
    allFImpl :: CTL (Context -> Sym)
             -> Tree (Node Context) -> SolverTask Sym
    allFImpl p (Tree.Node x xs) =
      Compound Disj [ formulateImpl p (mkTrace x [])
                    , Compound Conj (map (allFImpl p) xs)
                    ]

newtype Constraints = ConstrainedBy [Sym]
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Wrapper over tree nodes existing only because of its Eq instance, which
--   compares the symbolic expressions inside nodes and is used in to assemble
--   Sets of nodes while keeping their original ids.
newtype UNode a = MkUNode { _unwrapUNode :: Node a }

instance Eq a => Eq (UNode a) where
  MkUNode (Node _ x) == MkUNode (Node _ y) = x == y

instance Ord a => Ord (UNode a) where
  MkUNode (Node _ x) <= MkUNode (Node _ y) = x <= y

--------------------------------------------------------------------------------

-- | Schedule node problems for solving by translating them into SBV queries,
--   but keeping the combining operation that will need to be allied afterwards
scheduleImpl :: TVar (Map Text SBV.SInt32) -> TVar Int -> TQueue (Text, Maybe SMTResult)
          -> SolverTask Sym -> SolverTask (SBV.Query (Maybe Bool))
scheduleImpl env finished results = \case
  Atomic task       -> scheduleSingle task
  task@(Compound op tasks) ->
    case depth task of
      1 -> scheduleList op (concatMap atomicTasks tasks)
      _ -> Compound op (fmap (scheduleImpl env finished results) tasks)
  where
    scheduleSingle :: (Text, Sym) -> SolverTask (SBV.Query (Maybe Bool))
    scheduleSingle (n, expr) = Atomic . (n,) $
      groupQuery env finished results (Text.pack (show n), expr)

    -- | If the task only has atomic subtasks
    scheduleList :: Op -> [(Text, Sym)] -> SolverTask (SBV.Query (Maybe Bool))
    scheduleList op atoms =
      case op of
        -- we schedule a conjunction of atoms as a single query
        Conj ->
          let name = "{ " <> mconcat (map fst atoms)  <> " }"
              expr = conjoin . map snd $ atoms
          in Atomic (name, groupQuery env finished results (name, expr))
        -- whereas a disjunction is scheduled as multiple: one query per disjunct
        Disj -> Compound Disj $ map scheduleSingle atoms

schedule :: TVar (Map Text SBV.SInt32) -> TVar Int -> TQueue (Text, Maybe SMTResult)
          -> SolverTask Sym -> [(Text, SBV.Query (Maybe Bool))]
schedule env finished results =
  atomicTasks . scheduleImpl env finished results

prove :: Theorem -> Constraints -> IO [(Text, Maybe SMTResult)]
prove thm@(MkTheorem task) cs = do
  env <- newTVarIO (Map.empty)
  results <- newTQueueIO
  finishedCount <- newTVarIO 0
  let qs = schedule env finishedCount results task
  _ <- SBV.satConcurrentWithAll SBV.z3 (map snd qs) (prepare env cs thm)
  atomically $ do
    finished <- (== size task) <$> readTVar finishedCount
    case finished of
      True  -> flushTQueue results
      False -> retry
  -- let solved = map (second (maybe unknownFatal id)) $ answers
  --     (sats, unsats) = partition (isSat . snd) solved
  -- pure $
  --   case validity of
  --     AllSat ->
  --       case unsats of
  --         []    -> Proved thm
  --         (x:_) -> Falsifiable thm x
  --     AllUnsat ->
  --       case sats of
  --         []    -> Proved thm
  --         (x:_) -> Falsifiable thm x
  where unknownFatal = error "Impossible happened: prover said unknown!"

-- | Prove a theorem under constraints.
--   Concurrently run several solvers
-- prove :: Theorem -> Constraints -> IO [(NodeId, Maybe SMTResult)]
-- prove thm@(MkTheorem exprs) cs = do
--   env <- newTVarIO (Map.empty)
--   results <- newTQueueIO
--   finishedCount <- newTVarIO 0
--   let qs = fmap (nodeQuery env finishedCount results) exprs
--   _ <- SBV.satConcurrentWithAll SBV.z3 qs (prepare env cs thm)
--   atomically $ do
--     finished <- (== length exprs) <$> readTVar finishedCount
--     case finished of
--       True  -> flushTQueue results
--       False -> retry
--   -- let solved = map (second (maybe unknownFatal id)) $ answers
--   --     (sats, unsats) = partition (isSat . snd) solved
--   -- pure $
--   --   case validity of
--   --     AllSat ->
--   --       case unsats of
--   --         []    -> Proved thm
--   --         (x:_) -> Falsifiable thm x
--   --     AllUnsat ->
--   --       case sats of
--   --         []    -> Proved thm
--   --         (x:_) -> Falsifiable thm x
--   where unknownFatal = error "Impossible happened: prover said unknown!"


-- | Prepare the shared proving environment:
--   * Gather free symbolic variables (@SAny) from the theorem statement
--   * Add constrains to the environment
prepare :: TVar (Map Text SBV.SInt32) -> Constraints -> Theorem -> SBV.Symbolic ()
prepare env (ConstrainedBy cs) (MkTheorem tasks) = do
  vars <- createSym $
    Set.toList $ gatherFree (conjoin . map snd $ atomicTasks tasks)
  liftIO . atomically $ writeTVar env vars
  pre <- toSMT vars cs
  SBV.constrain pre

-- | An SMT-problem constructed from a single trace node
nodeQuery :: TVar (Map Text SBV.SInt32) -> TVar Int -> TQueue (NodeId, Maybe SMTResult)
          -> Node Sym -> SBV.Query (Maybe Bool)
nodeQuery env finished results (Node n expr) = do
  vars <- liftIO $ readTVarIO env
  SBV.constrain =<< toSMT vars [expr]
  SBV.checkSat >>= \case
    SBV.Unk -> do
      liftIO . atomically $ do
        writeTQueue results $ (n, Nothing)
        modifyTVar finished (+1)
      pure Nothing
    _ -> SBV.getSMTResult >>= \case
      (SBV.Satisfiable _ yes) -> do
        values <- traverse SBV.getValue vars
        liftIO . atomically $ do
          writeTQueue results $ (n, Just . Satisfiable . MkSMTModel $ values)
          modifyTVar finished (+1)
        pure $ Just True
      (SBV.Unsatisfiable _ _) -> do
        liftIO . atomically $ do
          writeTQueue results $ (n, Just Unsatisfiable)
          modifyTVar finished (+1)
        pure $ Just False
      _ -> error "not implemented"


-- | An SMT-problem constructed from a group of nodes
groupQuery :: TVar (Map Text SBV.SInt32) -> TVar Int -> TQueue (Text, Maybe SMTResult)
          -> (Text, Sym) -> SBV.Query (Maybe Bool)
groupQuery env finished results (n, expr) = do
  vars <- liftIO $ readTVarIO env
  SBV.constrain =<< toSMT vars [expr]
  SBV.checkSat >>= \case
    SBV.Unk -> do
      liftIO . atomically $ do
        writeTQueue results $ (n, Nothing)
        modifyTVar finished (+1)
      pure Nothing
    _ -> SBV.getSMTResult >>= \case
      (SBV.Satisfiable _ _) -> do
        values <- traverse SBV.getValue vars
        liftIO . atomically $ do
          writeTQueue results $ (n, Just . Satisfiable . MkSMTModel $ values)
          modifyTVar finished (+1)
        pure $ Just True
      (SBV.Unsatisfiable _ _) -> do
        liftIO . atomically $ do
          writeTQueue results $ (n, Just Unsatisfiable)
          modifyTVar finished (+1)
        pure $ Just False
      _ -> error "not implemented"
