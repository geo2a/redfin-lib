{-# LANGUAGE DeriveAnyClass #-}

module ISA.Types.Symbolic.Trace.Property
  ( Property(..), Validity(..), Constraints(..), Theorem(..), formulate, prove, Proof(..)
  ) where

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.DeepSeq            (force)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Bifunctor
import           Data.Foldable              (foldl')
import           Data.IORef
import           Data.List                  (partition)
import           Data.List                  (partition)
import           Data.List.Extra            (nubOrdOn)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, isJust, listToMaybe)
import qualified Data.SBV                   as SBV
import qualified Data.SBV.Control           as SBV
import qualified Data.SBV.Internals         as SBV
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time.Clock            (NominalDiffTime)
import qualified Data.Tree                  as Tree
import           Data.Typeable
import           GHC.Generics
import           Prelude                    hiding (not)

import           ISA.Types
import           ISA.Types.CTL
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Symbolic.Trace


data Validity = AllSat | AllUnsat
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Symbolic properties on program traces
data Property = InWhole Validity Key (Sym -> Sym)
             -- ^ Consider the value of the key in every node of the trace
              | InLeafs Validity Key (Sym -> Sym)
             -- ^ Only consider leaf nodes

deriving instance Typeable Property

instance Show Property where
  show = \case InWhole v k p -> "Whole " <> show v <> " " <> show k <> " <function>"
               InLeafs v k p -> "Leafs " <> show v <> " " <> show k <> " <function>"

-- | A theorem is a conjunction of a set of symbolic expressions
--   We keep the expressions in the nodes they come from for error reporting
data Theorem = MkTheorem Validity (Set (Node Sym))
  deriving (Generic, ToJSON, FromJSON)

instance Show Theorem where
  show (MkTheorem _ exprs) = unlines $
    map (show . _nodeBody) . Set.toList $ exprs

-- | Result of @Theorem proving
data Proof = Proved Theorem
           | Falsifiable Theorem (NodeId, SMTResult)
           deriving (Generic, ToJSON, FromJSON)

instance Show Proof where
  show = \case
    Proved _ -> "Q.E.D"
    Falsifiable theorem (n, contra) ->
      "Falsifiable! Counterexample caused by node " <> show n <> ": \n" <> show contra

formulate :: Property -> Trace Context -> Either Text Theorem
formulate p trace = case p of
  InWhole validity key consider -> formulateWhole validity key consider trace
  InLeafs validity key consider -> formulateLeafs validity key consider trace

newtype Constraints = ConstrainBy [Sym]
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Wrapper over tree nodes existing only because of its Eq instance, which
--   compares the symbolic expressions inside nodes and is used in to assemble
--   Sets of nodes while keeping their original ids.
newtype UNode a = MkUNode { _unwrapUNode :: Node a }

instance Eq a => Eq (UNode a) where
  MkUNode (Node _ x) == MkUNode (Node _ y) = x == y

instance Ord a => Ord (UNode a) where
  MkUNode (Node _ x) <= MkUNode (Node _ y) = x <= y

formulateWhole :: Validity -> Key -> (Sym -> Sym) -> Trace Context -> Either Text Theorem
formulateWhole validity key consider (Trace tree) =
  let rootHasIt = getBinding key (_nodeBody $ Tree.rootLabel tree)
  in case rootHasIt of
       Nothing -> Left $ "Error: root node does not bind " <> Text.pack (show key)
       Just _ -> Right $
         MkTheorem validity . Set.map _unwrapUNode $
           Tree.foldTree (\x acc -> Set.insert (considerNode x) (mconcat acc)) tree
  where
    considerNode :: Node Context -> UNode Sym
    considerNode = MkUNode . fmap consider . fmap (maybe false id . getBinding key)

-- | Create a symbolic formula by considering (applying @consider)
--   the symbolic expressions bound to a specific @key in every leaf
--   of a trace and conjoining the results
formulateLeafs :: Validity -> Key -> (Sym -> Sym) -> Trace Context -> Either Text Theorem
formulateLeafs validity key consider trace = do
  let (haveTheKey, missing) =
        partition (isJust . _nodeBody) . map (getBinding key <$>) . leafs $ trace
  case null missing of
    False -> Left $
      "Error: nodes " <> Text.pack (show $ map _nodeId missing) <> " don't bind key " <>
      Text.pack (show key)
    True ->
      let subjects = nubOrdOn _nodeBody $ map (maybe false id <$>) haveTheKey
      in Right . MkTheorem validity . Set.fromList . map (consider <$>) $ subjects

--------------------------------------------------------------------------------

-- | Prove a theorem under constraints.
--   Concurrently run several solvers
prove :: Theorem -> Constraints -> IO Proof
prove thm@(MkTheorem validity exprs) cs = do
  env <- newTVarIO (Map.empty)
  results <- newTQueueIO
  finishedCount <- newTVarIO 0
  let qs = map (nodeQuery env finishedCount results) (Set.toList exprs)
  _ <- SBV.satConcurrentWithAll SBV.z3 qs (prepare env cs thm)
  answers <- atomically $ do
    finished <- (== Set.size exprs) <$> readTVar finishedCount
    case finished of
      True  -> flushTQueue results
      False -> retry
  let solved = map (second (maybe unknownFatal id)) $ answers
      (sats, unsats) = partition (isSat . snd) solved
  pure $
    case validity of
      AllSat ->
        case unsats of
          []    -> Proved thm
          (x:_) -> Falsifiable thm x
      AllUnsat ->
        case sats of
          []    -> Proved thm
          (x:_) -> Falsifiable thm x

  where unknownFatal = error "Impossible happened: prover said unknown!"

-- | Prepare the shared proving environment:
--   * Gather free symbolic variables (@SAny) from the theorem statement
--   * Add constrains to the environment
prepare :: TVar (Map Text SBV.SInt32) -> Constraints -> Theorem -> SBV.Symbolic ()
prepare env (ConstrainBy cs) (MkTheorem _ exprs) = do
  vars <- createSym . Set.toList $
    case not (Set.null exprs) of
      False -> error "empty theorem!"
      True  -> gatherFree (conjoin . map _nodeBody $ Set.toList exprs)
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
