{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Dependencies
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Compute data dependencies of programs
--
-----------------------------------------------------------------------------

module ISA.Backend.Dependencies
  (Reads(..), Writes(..), dependencies, dependsOn) where

import           Control.Selective
import           Data.Bifunctor
import           Data.Either       (partitionEithers)
import           Data.List

import           FS

-- | A datatype marking a read-dependency
newtype Reads a = Reads a
  deriving (Show, Eq)

-- | A datatype marking a write-dependency
newtype Writes a = Writes a
  deriving (Show, Eq)

trackingRead :: key -> Over [Either key key] a
trackingRead key = Over [Left key]

trackingWrite :: key -> Over [Either key key] a -> Over [Either key key] a
trackingWrite key producer = producer *> Over [Right key]

-- | Extract input and output data-dependencies of a computation
dependencies :: FS key Selective '[Any] a
     -> (Reads [key], Writes [key])
dependencies task =
    bimap Reads Writes . partitionEithers . getOver $
    task trackingRead trackingWrite

-- | Check if a computation depends on specified keys
dependsOn :: Eq key => Reads [key] -> Writes [key] -> FS key Selective '[Any] a -> Bool
dependsOn (Reads reads) (Writes writes) computation =
  let (Reads readDeps, Writes writeDeps) = dependencies computation
  in (not . null $ intersect reads readDeps) || (not . null $ intersect writes writeDeps)
