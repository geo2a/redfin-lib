-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Backend.Dependencies
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Module desription
--
-----------------------------------------------------------------------------

module ToyRISC.Backend.Dependencies
  (dependencies) where

import           Control.Selective
import           Data.Either       (partitionEithers)

import           FS
import           ToyRISC.Types

trackingRead :: key -> Over [Either key key] a
trackingRead key = Over [Left key]

trackingWrite :: key -> Over [Either key key] a -> Over [Either key key] a
trackingWrite key producer = producer *> Over [Right key]

-- | Extract input and output data-dependencies of a computation
dependencies :: Value a => FS key Selective Value a
     -> ([key], [key])
dependencies task =
    partitionEithers . getOver $
    task trackingRead trackingWrite
