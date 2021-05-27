{- |
 Module     : ISA.Types.Symbolic.SMT.Solving
 Copyright  : (c) Georgy Lukyanov 2019
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 SMT solving
-}
module ISA.Types.Symbolic.SMT.Solving (sat) where

import Control.Concurrent.STM hiding (check)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.SBV.Internals as SBV
import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV
import Data.Text (Text)
import Data.Time.Clock 

import ISA.Types.SBV
import ISA.Types.Symbolic
import ISA.Types.Symbolic.Address
import ISA.Types.Symbolic.SMT.Problem hiding (_vars)
import ISA.Types.Symbolic.SMT.Translation

-- data SymExecStats = MkSymExecStats {_timing :: NominalDiffTime}
--     deriving (Show)


-- | Environment shared by concurrent solver tasks
data Env = MkEnv
    { _vars :: TVar (Map Text SBV.SInt32)
    , _results :: IntMap (TMVar (Sym, SMTResult))
    }

{- | Prepare the shared solving environment
   * Translate 'Sym' variables into SBV variables
   * Prepare shared data structures
-}
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
                  error "solver said unknown"
                _ -> do
                  SBV.getSMTResult >>= \case
                        (SBV.Satisfiable _ _) -> do
                            values <- traverse SBV.getValue vars
                            liftIO . atomically $ do
                                putTMVar resultBox $ (expr, Satisfiable (MkSMTModel values))
                        (SBV.Unsatisfiable _ _) -> do
                            liftIO . atomically $ do
                                putTMVar resultBox $ (expr, Unsatisfiable)
                        _ -> error "unsupported solver response"

{- | From a solving schedule, produce a list of queries for the solver and a
   schedule of results.
-}
produce :: Env -> Task ([(Address, Sym)], Sym) -> [SBV.Query ()]
produce env = go []
  where
    go acc = \case
        Atom task -> atomicQuery env task : acc
        Conjunct xs -> concatMap (go acc) xs

-- | Take all 'TMVar's in the schedule --- awaiting the results
consume :: Env -> STM (IntMap (Sym, SMTResult))
consume env = traverse takeTMVar (_results env)

{- | Check satisfiability of a 'Problem'
   Return an empty list if all nodes are unsats and the satisfiable nodes otherwise
-}
sat :: Problem Sym ([(Address, Sym)], Sym) -> IO Solution
sat (MkProblem symVars task) = do
    let tasks = flatten $ task
        stateIds = map fst tasks
        exprs = map (snd . snd) tasks
    vars <- liftIO $ newTVarIO mempty
    resultBoxes <-
        liftIO $
            foldM
                (\acc k -> newEmptyTMVarIO >>= \v -> pure $ IntMap.insert k v acc)
                IntMap.empty
                stateIds
    let env = MkEnv vars resultBoxes
    let qs = produce env task
    startTime <- getCurrentTime
    sbvOutput <- SBV.satConcurrentWithAll solver qs (prepare symVars env)
    solutions <- atomically (consume env)
    finishTime <- getCurrentTime
    let stats = mkStats (map (\(_, x, _) -> x) sbvOutput)
    print (stats {_totalTime = diffUTCTime finishTime startTime})
    pure (MkSolution solutions stats)

solver :: SBV.SMTConfig
solver =
    SBV.z3
        { SBV.verbose = False
        -- , SBV.redirectVerbose = Just "logs.smt2"
        , SBV.printBase = 10
        }

