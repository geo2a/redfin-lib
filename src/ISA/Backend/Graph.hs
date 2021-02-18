{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Graph
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Calculate a control-flow graph of a program
-----------------------------------------------------------------------------
module ISA.Backend.Graph (cfg, writeCfgDot) where

import qualified Algebra.Graph                as Alga
import qualified Algebra.Graph.Export.Dot     as Alga
import           Control.Selective
import           Data.Int
import qualified Data.Map                     as Map
import qualified Debug.Trace                  as Debug
import           Polysemy
import           Polysemy.Error
import           Polysemy.State

import           ISA.Assembly
import           ISA.Backend.Graph.BasicBlock
import           ISA.Backend.Simulation
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Instruction

cfg :: Script -> Alga.Graph (Maybe (Block (Data Int32)))
cfg src =
  let blocks = basicBlocks src
      ids = map (\(MkBlock a is _) -> map fst $ zip [a..] is) blocks
      dict = Map.fromList $ zip ids blocks
  in Alga.overlays . concatMap (Debug.trace (show dict) $ mkEdges dict) $ blocks
  where mkEdges dict b@(MkBlock a _ Nothing)       = [Alga.edge (Just b) Nothing]
        mkEdges dict b@(MkBlock a _ (Just (l, r))) =
          [ Alga.edge (Just b) (find l dict)
          , Alga.edge (Just b) (find r dict) ]
        find :: Address -> Map.Map [Address] (Block (Data Int32)) -> Maybe (Block (Data Int32))
        find k =
          Map.foldrWithKey (\xs v acc -> if elem k xs then Just v else acc) Nothing

cfgLayout :: Script -> Alga.Graph (Maybe Address)
cfgLayout src =
  let blocks = basicBlocks src
  in Alga.overlays . concatMap mkEdges $ blocks
  where mkEdges :: Block a -> [Alga.Graph (Maybe Address)]
        mkEdges (MkBlock a _ Nothing)       = [Alga.edge (Just a) Nothing]
        mkEdges (MkBlock a _ (Just (l, r))) = [ Alga.edge (Just a) (Just l)
                                              , Alga.edge (Just a) (Just r) ]

writeCfgDot :: FilePath -> Script -> IO ()
writeCfgDot path src =
  writeFile path . Alga.exportAsIs . fmap p $ cfg src
  where p = \case Nothing -> "⊥"
                  Just b  -> show b
