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
  ( dependencies
  , programDataGraph
  , drawGraph) where

import           Algebra.Graph
import           Algebra.Graph.Export.Dot
import           Control.Arrow            (second)
import           Data.Either              (partitionEithers)
import           Data.Maybe               (fromJust)
import qualified Data.Set                 as Set

import           FS
import           ISA.Selective
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Instruction

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

type InstructionLabel = String

-- | Compute static data flow graph of an instruction. In case of supplying a
--   monadic, i.e. data-dependent instruction, 'Nothing' is returned.
--
-- Since no data requiring simulation is performed, the semantics metalanguage
-- terms are mocked: 'read' becomes 'const 0' and 'write' is simply ignored.
instructionGraph :: Value a => (Address, Instruction a)
                            -> Maybe (Graph (Either Key InstructionLabel))
instructionGraph (addr, instr) = do
    let (ins, outs) = dependencies (instructionSemanticsS instr)
    let instrInfo = instructionLabel
    pure $ overlay (star (Right instrInfo) (map Left outs))
                   (transpose $ star (Right instrInfo) (map Left ins))
    where instructionLabel = show addr <> "|" <> show instr

-- | Serialise data flow graph as a .dot string
drawGraph :: Graph (Either Key InstructionLabel) -> String
drawGraph g = export style g
  where
    style = defaultStyleViaShow
        { vertexName = \v -> "v" ++ show (fromJust $ Set.lookupIndex v names)
        , vertexAttributes = \x -> case x of
            Left  k      -> [ "shape"  := "circle"
                            , "label"  := show k ]
            Right i -> [ "shape" := "record"
                            , "label" := i ] }
    names = vertexSet g
    -- instructionLabel a i = fromString (show a <> "|" <> show i)

-- | Compute static data flow graph of a program. In case of supplying a
--   monadic, i.e. data-dependent instruction, 'Nothing' is returned.
programDataGraph :: Value a => [(Address, Instruction a)]
                            -> Maybe (Graph (Either Key InstructionLabel))
programDataGraph p =
    let p' = map (second id) p
    in  foldl go (Just empty) (map instructionGraph p')
    where go _   Nothing = Nothing
          go acc g       = overlay <$> acc <*> g
