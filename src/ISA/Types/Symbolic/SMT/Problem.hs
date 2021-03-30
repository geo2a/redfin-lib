-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.SMT.Problem
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-----------------------------------------------------------------------------

module ISA.Types.Symbolic.SMT.Problem
  ( Atomic(..), Task(..), Problem(..), task, tasks, flatten
  ) where

newtype Atomic a = MkAtomic (Int, a)
  deriving (Show, Functor)

data Problem a b = MkProblem { _vars :: [a]
                             , _task :: Task b
                             } deriving Show

-- | A task for the solver. 'Atom' constructors represent
--   atomic tasks, usually associated with single trace nodes
data Task a = Atom (Atomic a)
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
      Conjunct xs            -> concatMap flatten xs
