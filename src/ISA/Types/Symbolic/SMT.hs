{-# LANGUAGE TupleSections #-}
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
    ( -- statistics after running symbolic execution with solving
      SymExecStats (..)

      -- get the list of free variables in a symbolic expression
    , findFreeVars, gatherFree

    -- declare variables/expressions as symbolic
    , createSym

    -- helper functions
    , conjoin

    , symAddress, symInt32, toSMT

    -- Proving
    , prove
    ) where

import           Control.Concurrent.STM      hiding (check)
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Functor
import           Data.Int
import qualified Data.IntMap                 as IntMap
import           Data.List
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.SBV.Control            as SBV
import qualified Data.SBV.Trans              as SBV
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Time.Clock             (NominalDiffTime)
import           Data.Traversable
import           Debug.Trace
import           GHC.Stack
import           Prelude                     hiding (not)

import           ISA.Backend.Symbolic.Zipper hiding (Context)
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Prop
import           ISA.Types.SBV
import           ISA.Types.SBV.SFunArray     (SFunArray)
import qualified ISA.Types.SBV.SFunArray     as SFunArray
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address
import           ISA.Types.Symbolic.Property
import           ISA.Types.Tree

data SymExecStats = MkSymExecStats { _timing :: NominalDiffTime }
  deriving Show

-- | Find all free symbolic variables (SAny) in the context
findFreeVars :: Context Sym -> Set Sym
findFreeVars ctx =
  let fromBindings = mconcat . fmap gatherFree . Map.elems $ _bindings ctx
      fromStore = mconcat . fmap gatherFree . Map.elems $ _store ctx
      fromConstraints = gatherFree . conjoin . map snd $ _constraints ctx
  in fromBindings <> fromStore <> fromConstraints

-- | Walk through a symbolic expression gathering up the free variables.
gatherFree :: Sym -> Set Sym
gatherFree = \case
  c@(SAny _)   -> Set.singleton c
  (SPointer p) -> gatherFree p
  (SAdd l r)   -> gatherFree l <> gatherFree r
  (SSub l r)   -> gatherFree l <> gatherFree r
  (SMul l r)   -> gatherFree l <> gatherFree r
  (SDiv l r)   -> gatherFree l <> gatherFree r
  (SMod l r)   -> gatherFree l <> gatherFree r
  (SAbs l)     -> gatherFree l
  (SNot c)     -> gatherFree c
  (SOr l r)    -> gatherFree l <> gatherFree r
  (SAnd l r)   -> gatherFree l <> gatherFree r
  (SEq l r)    -> gatherFree l <> gatherFree r
  (SGt l r)    -> gatherFree l <> gatherFree r
  (SLt l r)    -> gatherFree l <> gatherFree r
  (SConst _)   -> mempty

-- | Create existential symbolic variables for each of 'SAny''s in the input.
createSym :: (HasCallStack, SBV.MonadSymbolic m)
          => [Sym] -> m (Map.Map Text SBV.SInt32)
createSym cs = do
  pairs <- traverse createSymPair cs
  pure $ Map.fromList pairs
    where createSymPair (SAny name) = do
            v <- SBV.sInt32 (Text.unpack name)
            pure (name, v)
          createSymPair _ = error "Non-variable encountered."

-- | Convert a list of path constraints to a symbolic value the SMT solver can solve.
--   Each constraint in the list is conjoined with the others.
toSMT :: MonadIO m
      => SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> [Sym] -> m SBV.SBool
toSMT mem vars cs = do
  let smts = process $ map (symBool mem vars) cs
  pure $ conjoinSBV smts

  where process [] = []
        process (x:xs) =
          case x of
            Nothing -> process xs
            Just y  -> y:process xs

-- | Convert a symbolic expression into an SBV 'Int32'
symInt32 :: SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> Sym -> Maybe SBV.SInt32
symInt32 mem vars = \case
  (SConst (CInt32 i)) -> Just (SBV.literal i)
  SAny x              -> Map.lookup x vars
  SPointer p          -> do
    v <- symAddress vars (MkAddress (Right p)) >>=
                         Just . SFunArray.readArray mem
    -- trace ("Symbolic pointer access: " <> show (p, v) <> "\n") $
    pure v
  SAdd l r            -> (+) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SSub l r            -> (-) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SMul l r            -> (*) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SDiv l r            -> (SBV.sDiv) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SMod l r            -> (SBV.sMod) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SAbs x              -> abs <$> symInt32 mem vars x
  _                   -> Nothing

-- | Convert a symbolic expression into an SBV 'Bool'
symBool :: SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> Sym -> Maybe SBV.SBool
symBool mem vars = \case
  (SConst (CBool b)) -> Just $ SBV.literal b
  (SConst _        ) -> Nothing
  SAny _             -> Nothing
  (SEq l r)          -> (SBV..==) <$> symInt32 mem vars l <*> symInt32 mem vars r
  (SGt  l r)         -> (SBV..>)  <$> symInt32 mem vars l <*> symInt32 mem vars r
  (SLt  l r)         -> (SBV..<)  <$> symInt32 mem vars l <*> symInt32 mem vars r
  (SAnd l r)         -> (SBV..&&) <$> symBool mem vars l  <*> symBool mem vars r
  (SOr  l r)         -> (SBV..||) <$> symBool mem vars l  <*> symBool mem vars r
  (SNot x  )         -> SBV.sNot  <$> symBool mem vars x
  _                  -> Nothing

-- | Interpret a  symbolic memory address into an SBV symbolic integer
--   We do not allow nested pointers
symAddress :: Map.Map Text SBV.SInt32 -> Address -> Maybe SBV.SInt32
symAddress vars = \case
  MkAddress (Left (CAddress concrete)) -> Just (SBV.literal (fromIntegral concrete))
  MkAddress (Right sym) ->
    -- trace ("Symbolic address: " <> show sym <> "\n") $
    symInt32 (SFunArray.sListArray 0 []) vars sym

conjoinSBV :: [SBV.SBool] -> SBV.SBool
conjoinSBV = foldr (\x y -> (SBV..&&) x y) (SBV.sTrue)
------------------------ -----------------------------------------------------

-- | A solver schedule. 'Literal' constructors represent
--   atomic tasks
data Schedule a = Literal a
                | Conjunct [Schedule a]
                | Disjunct [Schedule a]
                deriving (Show, Functor)

flatten :: Schedule a -> [a]
flatten = go []
  where
    go acc = \case
      Literal x   -> x : acc
      Conjunct xs -> concatMap flatten xs
      Disjunct xs -> concatMap flatten xs

formulate :: Trace -> Theorem -> Schedule (Int, ([(Address, Sym)], Sym))
formulate tr = \case InWhole p -> formulateWhole p tr
                     InLeafs p -> formulateLeafs p tr

-- | Prepare a solving schedule considering only leafs of the trace.
--   Return 'false' if the trace is malformed
formulateLeafs :: Property -> Trace -> Schedule (Int, ([(Address, Sym)],Sym))
formulateLeafs (Property prop) trace =
  let ids = leafs $ _layout trace
      states = _states trace
  in Conjunct ( map Literal . zip ids
              . map (\i -> maybe ([], false) atState $ IntMap.lookup i states) $ ids)
  where
    atState :: Context Sym -> ([(Address, Sym)], Sym)
    atState ctx =
          let eqs = foldr (&&&) true
                  . map (\(name, expr) -> SAny name === expr)
                  . Map.assocs $ _store ctx
              pc = _pathCondition ctx
          in  (dumpMemory ctx, pc &&& eqs &&& prop ctx)

-- | Prepare a solving schedule considering the whole trace
--   Return 'false' if the trace is malformed
formulateWhole :: Property -> Trace -> Schedule (Int, ([(Address, Sym)],Sym))
formulateWhole (Property prop) trace =
  let ids = keys $ _layout trace
      states = _states trace
  in Conjunct ( map Literal . zip ids
              . map (\i -> maybe ([], false) atState $ IntMap.lookup i states) $ ids)
  where atState ctx =
          let eqs = foldr (&&&) true
                  . map (\(name, expr) -> SAny name === expr)
                  . Map.assocs $ _store ctx
              pc = _pathCondition ctx
          in (dumpMemory ctx,  pc &&& eqs &&& prop ctx)

-- | From a solving schedule, produce a list of queries for the solver and a
--   schedule of results.
produce :: TVar (Map Text SBV.SInt32)
        -> Schedule (Int, ([(Address, Sym)], Sym)) -> IO ([SBV.Query ()], Schedule (Int, TMVar SMTResult))
produce env schedule =
  (\r -> (flatten (fmap (\(_, q, _) -> q) r), fmap (\(n, _, v) -> (n, v)) r)) <$> go schedule
  where
    go schedule =
      case schedule of
        Literal (nid, (mem, sym)) -> do
          vars <- readTVarIO env
          resultBox <- newEmptyTMVarIO
          let query = nodeQuery (symbolicMemory vars mem) env resultBox sym
          pure (Literal (nid, query, resultBox))
        Conjunct xs        -> Conjunct <$> mapM go xs
        Disjunct xs        -> Disjunct <$> mapM go xs

symbolicMemory :: Map Text SBV.SInt32 -> [(Address, Sym)] -> SFunArray SBV.Int32 SBV.Int32
symbolicMemory vars mem =
  SFunArray.sListArray 0 .
  map (second (maybe 42 id . symInt32 (SFunArray.sListArray 0 []) vars)) .
  map (first (maybe 42 id . symAddress vars)) $ mem

-- | Prepare the shared proving environment by communicating free variables to SBV
prepare :: Context Sym -> TVar (Map Text SBV.SInt32)
        -> Set Sym -> [Sym] -> SBV.Symbolic ()
prepare init env freeVars cs = do
  vars <- createSym $ Set.toList freeVars
  liftIO . atomically $ writeTVar env vars
  let initMemory = symbolicMemory vars $ dumpMemory init
  -- liftIO . atomically . writeTVar mem $ memory
  pre <- toSMT initMemory vars cs
  SBV.constrain pre

-- | Take all 'TMVar's in the schedule --- awaiting the results
consume :: Schedule (Int, TMVar SMTResult) -> STM (Schedule (Int, SMTResult))
consume schedule = case schedule of
  Literal (nid, var) -> Literal . (nid,) <$> takeTMVar var
  Conjunct xs        -> Conjunct <$> mapM consume xs
  Disjunct xs        -> Disjunct <$> mapM consume xs

-- | Check if a property is satisfiable on the given trace
--   Return an empty list if all nodes are unsats and the satisfiable nodes otherwise
sat :: Theorem -> Trace -> IO [(Int, SMTResult)]
sat prop trace = do
  let schedule = formulate trace prop
      freeVars = mconcat . map findFreeVars . IntMap.elems $ _states trace
  env <- newTVarIO mempty
  -- here we assume that the symbolic memory is 'read-only' which is not true.
  -- TODO: augment @Schedule@ with symbolic memory and use it accordingly
  let init = maybe emptyTraceError id . IntMap.lookup 0 $ _states trace
      preconditions = map snd $ _constraints init
  (qs, sch) <- produce env schedule
  _ <- SBV.satConcurrentWithAll SBV.z3 qs (prepare init env freeVars preconditions)
  solutions <- atomically (consume sch)
  pure . sortBy (comparing fst) . flatten $ solutions
  where emptyTraceError = error "empty trace"

-- | An SMT-problem constructed from a node
nodeQuery :: SFunArray SBV.Int32 SBV.Int32 -> TVar (Map Text SBV.SInt32)
          -> TMVar SMTResult -> Sym -> SBV.Query ()
nodeQuery memory env resultBox expr = do
  vars <- liftIO $ readTVarIO env
  SBV.constrain =<< toSMT memory vars [expr]
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

--------------------------------------------------------------------------------

-- | Prove a property by checking its negation for satisfiability
prove :: Theorem -> Trace -> IO Proof
prove prop trace =
  sat (negated prop) trace <&> filter (isSat . snd) >>= \case
    []     -> pure $ Proved prop
    contra -> pure $ Falsifiable prop contra
