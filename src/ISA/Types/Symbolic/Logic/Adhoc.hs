{-# LANGUAGE DeriveAnyClass #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.Logic.Adhoc
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- An ad-hoc logic for properties of symbolic simulation traces
--
-----------------------------------------------------------------------------

module ISA.Types.Symbolic.Logic.Adhoc
  where

import           Control.Concurrent.STM      hiding (check)
import           Control.Monad.IO.Class
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Bifunctor
import           Data.Int
import qualified Data.IntMap                 as IntMap
import           Data.Map                    (Map)
import           Data.Maybe
import qualified Data.SBV                    as SBV
import qualified Data.SBV.Control            as SBV
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           GHC.Generics
import           Prelude                     hiding (not)

import           ISA.Backend.Symbolic.Zipper
import           ISA.Types
import           ISA.Types.Context           (dumpMemory, getBinding)
import           ISA.Types.Key
import           ISA.Types.Prop
import           ISA.Types.SBV
import           ISA.Types.SBV.SFunArray     (SFunArray)
import qualified ISA.Types.SBV.SFunArray     as SFunArray
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Tree

-- | Syntax of the logic: we are interested in symbolic
--   qualities that hold for the whole trace, or in leafs
data Logic = InWhole (Context -> Sym)
           | InLeafs (Context -> Sym)

-- instance Num Sym where
--   x + y = SAdd x y
--   x - y = SSub x y
--   x * y = SMul x y
--   abs x = SAbs x
--   signum _ = error "Sym.Num: signum is not defined"
--   fromInteger x = SConst (CInt32 $ fromInteger x)
--   negate _ = error "Sym.Num: negate is not defined"
--   -- negate x = SSub 0 x

instance Num (Context -> Sym) where
  x + y = \s -> SAdd (x s) (y s)
  x - y = \s -> SSub (x s) (y s)
  x * y = \s -> SMul (x s) (y s)
  abs x = \s -> SAbs (x s)
  signum _ = error "(Context -> Sym).Num: signum is not defined"
  fromInteger x = const (SConst (CInt32 $ fromInteger x))
  negate _ = error "(Context -> Sym).Num: negate is not defined"

instance Boolean (Context -> Sym) where
  true = const true
  false = const false
  toBool = error "Boolean.(Context -> Sym): not implemented"
  fromBool b = const (fromBool b)
  not f = \s -> not (f s)
  p ||| q = \s -> p s ||| q s
  p &&& q = \s -> p s &&& q s

instance TryEq (Context -> Sym) where
  p === q = \s -> p s === q s

instance TryOrd (Context -> Sym) where
  lt p q = \s -> p s `lt` q s
  gt p q = \s -> p s `gt` q s

-- | Formulate a property considering the symbolic value of
--   a particular key. Return 'false' if the key is not bound
key :: Key -> (Context -> Sym)
key k = \ctx -> maybe false id (getBinding k ctx)

var :: Text -> (Context -> Sym)
var name = const (SAny name)

-----------------------------------------------------------------------------

formulate :: Trace -> Logic -> Schedule (Int, Sym)
formulate tr = \case InWhole _ -> undefined
                     InLeafs p -> formulateLeafs p tr

-- | Prepare a solving schedule considering only leafs of the trace.
--   Return 'false' if the trace is malformed
formulateLeafs :: (Context -> Sym) -> Trace -> Schedule (Int, Sym)
formulateLeafs prop trace =
  let ids = map fst . leafs $ _layout trace
      states = _states (resolvePointers trace)
  in Conjunct ( map Literal . zip ids
               . map (\i -> maybe false prop $ IntMap.lookup i states) $ ids)

-- | From a solving schedule, produce a list of queries for the solver and a
--   schedule of results.
produce :: TVar (SFunArray SBV.Int32 SBV.Int32) -> TVar (Map Text SBV.SInt32)
        -> Schedule (Int, Sym) -> IO ([SBV.Query ()], Schedule (Int, TMVar SMTResult))
produce mem vars schedule =
  (\r -> (flatten (fmap (\(_, q, _) -> q) r), fmap (\(n, _, v) -> (n, v)) r)) <$> go schedule
  where
    go schedule =
      case schedule of
        Literal (nid, sym) -> do
          resultBox <- newEmptyTMVarIO
          let query = nodeQuery mem vars resultBox sym
          pure (Literal (nid, query, resultBox))
        Conjunct xs        -> Conjunct <$> mapM go xs
        Disjunct xs        -> Disjunct <$> mapM go xs

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


-- | A list of symbolic expressions to serve as constraints for the state space
newtype Constraints = ConstrainedBy [Sym]
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Prepare the shared proving environment by communicating free variables to SBV
prepare :: Context -> TVar (SFunArray SBV.Int32 SBV.Int32) -> TVar (Map Text SBV.SInt32)
        -> Set Sym -> Constraints -> SBV.Symbolic ()
prepare init mem env freeVars (ConstrainedBy cs) = do
  vars <- createSym $ Set.toList freeVars
  liftIO . atomically $ writeTVar env vars
  let memory =
        SFunArray.sListArray 0 .
        map (bimap (fromJust . symAddress vars)
              (fromJust . symInt32 (SFunArray.sListArray 0 []) vars)) .
        dumpMemory $ init
  liftIO . atomically . writeTVar mem $ memory
  pre <- toSMT memory vars cs
  SBV.constrain pre

-- | Take all 'TMVar's in the schedule --- awaiting the results
consume :: Schedule (Int, TMVar SMTResult) -> STM (Schedule (Int, SMTResult))
consume schedule = case schedule of
  Literal (nid, var) -> Literal . (nid,) <$> takeTMVar var
  Conjunct xs        -> Conjunct <$> mapM consume xs
  Disjunct xs        -> Disjunct <$> mapM consume xs

-- | Flatten the schedule and apply it's node operations to the results
postprocess :: Schedule (Int, SMTResult) -> [(Int, SMTResult)]
postprocess schedule =
  let sats = postprocessImpl [] schedule
  in case schedule of
       Literal _  -> sats
       Conjunct _ -> if all (isSat . snd) sats then sats else []
       Disjunct _ -> if any (isSat . snd) sats then sats else []

postprocessImpl :: [(Int, SMTResult)] -> Schedule (Int, SMTResult) -> [(Int, SMTResult)]
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


-- | Check if a property is satisfiable on the given trace under the given constraints
--   Return an empty list if all nodes are unsats and the satisfiable nodes otherwise
-- sat :: Logic -> Trace -> Constraints -> IO [(Int, SMTResult)]
sat :: Logic -> Trace -> Constraints -> IO (Schedule (Int, SMTResult))
sat prop trace cs = do
  let schedule = formulate trace prop
      freeVars = gatherFree . conjoin . map snd . flatten $ schedule
  env <- newTVarIO mempty
  -- here we assume that the symbolic memory is 'read-only' which is not true.
  -- TODO: augment @Schedule@ with symbolic memory and use it accordingly
  mem <- newTVarIO $ SFunArray.sListArray 0 []
  let init = fromJust . IntMap.lookup 0 $ _states trace
  (qs, sch) <- produce mem env schedule
  _ <- SBV.satConcurrentWithAll SBV.z3 qs (prepare init mem env freeVars cs)
  solutions <- atomically (consume sch)
--  pure (postprocess solutions)
  pure solutions

-- | An SMT-problem constructed from a node
nodeQuery :: TVar (SFunArray SBV.Int32 SBV.Int32) -> TVar (Map Text SBV.SInt32)
          -> TMVar SMTResult -> Sym -> SBV.Query ()
nodeQuery mem env resultBox expr = do
  vars <- liftIO $ readTVarIO env
  memory <- liftIO $ readTVarIO mem
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
