{- |
 Module     : ISA.Types.Symbolic.SMT.Problem
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental
-}
module ISA.Types.Symbolic.SMT.Problem (
    Atomic (..),
    Task (..),
    Problem (..),
    Solution (..), Stats (..), mkStats,
    task,
    tasks,
    flatten,
) where

import Data.IntMap (IntMap)
import Data.List (sort)
import qualified Data.IntMap as IntMap
import Data.Time.Clock (NominalDiffTime)
import qualified GHC.Generics as GHC
import qualified Data.Aeson as Aeson

import ISA.Types.Symbolic
import ISA.Types.SBV

newtype Atomic a = MkAtomic (Int, a)
    deriving (Show, Functor)

data Problem a b = MkProblem
    { _vars :: [a]
    , _task :: Task b
    }
    deriving (Show)

-- | Solving statistics 
data Stats = MkStats
  { -- | time elapsed for every query.
    -- we currently can't associate timing to queries
    _timings :: [NominalDiffTime]
  , _totalTime :: NominalDiffTime
  , _avgTime :: NominalDiffTime
  , _medianTime :: NominalDiffTime
  }

deriving instance GHC.Generic Stats
instance Aeson.ToJSON Stats where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Stats

mkStats :: [NominalDiffTime] -> Stats
mkStats timings = 
  let nQueries = length timings
      total = sum timings
      median = case drop (nQueries `div` 2) . sort $ timings of
        [] -> 0
        (x:_) -> x
      mean = if nQueries > 0 then total / fromIntegral nQueries else 0
  in MkStats timings total mean median

instance Show Stats where
  show s = unlines 
    [ "Number of queries: " <> show (length (_timings s))
    , "Total time solving: " <> show (_totalTime s)
    , "Average time per query: " <> show (_avgTime s)
    -- , "Median time per query: " <> show (_medianTime s)
    ]

data Solution = MkSolution { _queries :: IntMap (Sym, SMTResult)
                           , _stats :: Stats
                           }
  deriving Show

deriving instance GHC.Generic Solution
instance Aeson.ToJSON Solution where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Solution

{- | A task for the solver. 'Atom' constructors represent
   atomic tasks, usually associated with single trace nodes
-}
data Task a
    = Atom (Atomic a)
    | Conjunct [Task a]
    deriving (Show, Functor)


task :: Atomic a -> Task a
task = Atom

tasks :: [Task a] -> Task a
tasks = Conjunct

flatten :: Task a -> [(Int, a)]
flatten = go []
  where
    go acc = \case
        Atom (MkAtomic (n, x)) -> (n, x) : acc
        Conjunct xs -> concatMap flatten xs
