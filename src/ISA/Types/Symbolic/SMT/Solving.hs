-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.SMT.Solving
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- SMT solving
--
-----------------------------------------------------------------------------

module ISA.Types.Symbolic.SMT.Solving
    (sat) where

import           Control.Concurrent.STM             hiding (check)
import           Control.Exception                  hiding (Overflow)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.IntMap                        (IntMap)
import qualified Data.IntMap                        as IntMap
import           Data.Map                           (Map)
import qualified Data.SBV.Internals                 as SBV
import qualified Data.SBV.Trans                     as SBV
import qualified Data.SBV.Trans.Control             as SBV
import           Data.Text                          (Text)
import           Data.Time.Clock                    (NominalDiffTime)

import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address
import           ISA.Types.Symbolic.SMT.Problem     hiding (_vars)
import           ISA.Types.Symbolic.SMT.Translation

data SymExecStats = MkSymExecStats { _timing :: NominalDiffTime }
  deriving Show

hasLocked :: String -> IO a -> IO a
hasLocked msg action =
  action `catches`
  [ Handler $ \exc@BlockedIndefinitelyOnSTM -> putStrLn ("[STM]: " ++ msg) >> throwIO exc
  ]


-- | Environment shared by concurrent solver tasks
data Env = MkEnv { _vars    :: TVar (Map Text SBV.SInt32)
                 , _results :: IntMap (TMVar SMTResult)
                 }

-- | Prepare the shared solving environment
--   * Translate 'Sym' variables into SBV variables
--   * Prepare shared data structures
prepare :: [Sym] -> Env -> SBV.Symbolic ()
prepare symVars env = do
  vars <- createSym symVars
  liftIO . atomically $
    writeTVar (_vars env) vars

-- | An SMT query constructed from a node, to be checked concurrently
atomicQuery :: Env -> Atomic ([(Address, Sym)], Sym) -> SBV.Query ()
atomicQuery env (MkAtomic (n, (mem, expr))) = do
  vars <- liftIO . readTVarIO $ _vars env
  let resultBoxes = _results env
  case (IntMap.!?) resultBoxes n of
    Nothing -> do
      liftIO . putStrLn $ "Query for state " <> show n <> ": no TMVar in the result map!"
      error "fatal error"
    Just resultBox -> do
      let c = toSMT (symbolicMemory vars mem) vars expr
      SBV.constrain c
      SBV.checkSat >>= \case
        SBV.Unk ->
          liftIO . atomically $ do
            putTMVar resultBox $ Unsatisfiable
        _ -> SBV.getSMTResult >>= \case
          (SBV.Satisfiable _ _) -> do
            values <- traverse SBV.getValue vars
            liftIO . atomically $ do
              putTMVar resultBox $ Satisfiable . MkSMTModel $ values
          (SBV.Unsatisfiable _ _) -> do
            liftIO . atomically $ do
              putTMVar resultBox $ Unsatisfiable
          _ -> liftIO . atomically $ do
                 putTMVar resultBox $ Unsatisfiable

-- | From a solving schedule, produce a list of queries for the solver and a
--   schedule of results.
produce :: Env -> Task ([(Address, Sym)], Sym) -> [SBV.Query ()]
produce env = go []
  where
    go acc = \case
      Atom task   -> atomicQuery env task : acc
      Conjunct xs -> concatMap (go acc) xs

-- | Take all 'TMVar's in the schedule --- awaiting the results
consume :: Env -> STM (IntMap SMTResult)
consume env = traverse takeTMVar (_results env)

-- | Check satisfiability of a 'Problem'
--   Return an empty list if all nodes are unsats and the satisfiable nodes otherwise
sat :: Problem Sym ([(Address, Sym)], Sym) -> IO (IntMap SMTResult)
sat (MkProblem symVars task) = do
  let stateIds = map fst . flatten $ task
  vars <- liftIO $ newTVarIO mempty
  resultBoxes <- liftIO $
    foldM (\acc k -> newEmptyTMVarIO >>= \v -> pure $ IntMap.insert k v acc)
    IntMap.empty stateIds
  let env = MkEnv vars resultBoxes
  let qs = produce env task
  _ <- SBV.satConcurrentWithAll SBV.z3 qs (prepare symVars env)
  solutions <- atomically (consume env)
  pure solutions
