{-# LANGUAGE DeriveAnyClass #-}

module ISA.Types.Symbolic.Trace.Property
  ( Property(..), Constraints(..), Theorem(..), formulate, prove
  ) where

import           Control.Concurrent.STM
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Foldable              (foldl')
import           Data.IORef
import           Data.List                  (partition)
import           Data.List.Extra            (nubOrdOn)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, isJust)
import qualified Data.SBV                   as SBV
import qualified Data.SBV.Control           as SBV
import qualified Data.SBV.Internals         as SBV
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time.Clock            (NominalDiffTime)
import qualified Data.Tree                  as Tree
import           GHC.Generics
import           Prelude                    hiding (not)

import           ISA.Types
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Symbolic.Trace

-- | Symbolic properties on program traces
data Property = InWhole Key (Sym -> Sym)
             -- ^ Consider the value of the key in every node of the trace
              | InLeafs Key (Sym -> Sym)
             -- ^ Only consider leaf nodes

-- | A theorem is a conjunction of a set of symbolic expressions
--   We keep the expressions in the nodes they come from for error reporting
newtype Theorem = MkTheorem (Set (Node Sym))
  deriving (Show, Generic, ToJSON, FromJSON)

formulate :: Property -> Trace Context -> Either Text Theorem
formulate p trace = case p of
  InWhole key consider -> formulateWhole key consider trace
  InLeafs key consider -> formulateLeafs key consider trace

newtype Constraints = ConstrainBy (Set Sym)
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Wrapper over tree nodes existing only because of its Eq instance, which
--   compares the symbolic expressions inside nodes and is used in to assemble
--   Sets of nodes while keeping their original ids.
newtype UNode a = MkUNode { _unwrapUNode :: Node a }

instance Eq a => Eq (UNode a) where
  MkUNode (Node _ x) == MkUNode (Node _ y) = x == y

instance Ord a => Ord (UNode a) where
  MkUNode (Node _ x) <= MkUNode (Node _ y) = x <= y

formulateWhole :: Key -> (Sym -> Sym) -> Trace Context -> Either Text Theorem
formulateWhole key consider (Trace tree) =
  let rootHasIt = getBinding key (_nodeBody $ Tree.rootLabel tree)
  in case rootHasIt of
       Nothing -> Left $ "Error: root node does not bind " <> Text.pack (show key)
       Just _ -> Right $
         MkTheorem . Set.map _unwrapUNode $
           Tree.foldTree (\x acc -> Set.insert (considerNode x) (mconcat acc)) tree
  where
    considerNode :: Node Context -> UNode Sym
    considerNode = MkUNode . fmap consider . fmap (maybe false id . getBinding key)

-- | Create a symbolic formula by considering (applying @consider)
--   the symbolic expressions bound to a specific @key in every leaf
--   of a trace and conjoining the results
formulateLeafs :: Key -> (Sym -> Sym) -> Trace Context -> Either Text Theorem
formulateLeafs key consider trace = do
  let (haveTheKey, missing) =
        partition (isJust . _nodeBody) . map (getBinding key <$>) . leafs $ trace
  case null missing of
    False -> Left $
      "Error: nodes " <> Text.pack (show $ map _nodeId missing) <> " don't bind key " <>
      Text.pack (show key)
    True ->
      let subjects = nubOrdOn _nodeBody $ map (maybe false id <$>) haveTheKey
      in Right . MkTheorem . Set.fromList . map (consider <$>) $ subjects

--------------------------------------------------------------------------------

-- querys :: Theorem -> SBV.Symbolic [SBV.Query SBV.SBool]
-- querys (MkTheorem exprs) = do
--   vars <- createSym . Set.toList $
--     case Set.null exprs of
--       False -> error "empty theorem!"
--       True  -> gatherFree (_nodeBody . head $ Set.toList exprs)
--   pure $ map (toSMT vars) (map (:[]) $ Set.toList $ Set.map _nodeBody exprs)

prepare :: TVar (Map Text SBV.SInt32) -> Constraints -> Theorem -> SBV.Symbolic ()
prepare env (ConstrainBy cs) (MkTheorem exprs) = do
  vars <- createSym . Set.toList $
    case not (Set.null exprs) of
      False -> error "empty theorem!"
      True  -> gatherFree $ SAnd (conjoin (Set.toList cs))
                                 (_nodeBody . head $ Set.toList exprs)
  liftIO . atomically $ writeTVar env vars
  pre <- toSMT vars (Set.toList cs)
  SBV.constrain pre

nodeQuery :: TVar (Map Text SBV.SInt32) -> Node Sym -> SBV.Query (Maybe SMTResult)
nodeQuery env (Node n expr) = do
  vars <- liftIO $ readTVarIO env
  SBV.constrain =<< toSMT vars [expr]
  SBV.checkSat >>= \case
    SBV.Unk -> pure Nothing
    _ -> SBV.getSMTResult >>= \case
      (SBV.Satisfiable _ yes) ->
        pure $ (Just . Satisfiable . MkSMTModel $ SBV.modelAssocs yes)
      (SBV.Unsatisfiable _ _) ->
        pure $ Just $ Unsatisfiable
      _ -> error "not implemented"

prove :: Theorem -> Constraints -> IO ()
prove thm@(MkTheorem exprs) cs = do
  env <- newTVarIO (Map.empty)
  let qs = map (nodeQuery env) (Set.toList exprs)
  z <- SBV.satConcurrentWithAll SBV.z3 qs (prepare env cs thm)
  print z
-- prove :: Theorem -> IO ()
-- prove thm = do
--   qs <- querys thm
