{-# LANGUAGE RecordWildCards     #-}
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
import           Algebra.Graph.Export         hiding (unlines)
import qualified Algebra.Graph.Export         as Export
import           Algebra.Graph.Export.Dot     (Attribute (..), Style (..))
import qualified Algebra.Graph.Export.Dot     as Alga
import           Control.Selective
import           Data.Int
import           Data.List                    (intersperse)
import qualified Data.Map                     as Map
import           Data.String                  hiding (unlines)
import qualified Debug.Trace                  as Debug
import           Polysemy
import           Polysemy.Error
import           Polysemy.State

import           ISA.Assembly                 (Script, assemble)
import           ISA.Backend.Graph.BasicBlock
import           ISA.Backend.Simulation
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Instruction
import           ISA.Types.ZeroOneTwo

cfg :: Script -> Alga.Graph (Maybe (Block (Data Int32)))
cfg src =
  let blocks = basicBlocks src
      ids = map (\(MkBlock a is _ _) -> map fst $ zip [a..] is) blocks
      dict = Map.fromList $ zip ids blocks
  in Alga.overlays . concatMap (mkEdges dict) $ blocks
  where mkEdges dict b@(MkBlock a _ _ Zero)       = [Alga.edge (Just b) Nothing]
        mkEdges dict b@(MkBlock a _ _ (One n)) =
          [ Alga.edge (Just b) (find n dict) ]
        mkEdges dict b@(MkBlock a _ _ (Two l r)) =
          [ Alga.edge (Just b) (find l dict)
          , Alga.edge (Just b) (find r dict) ]
        find :: Address -> Map.Map [Address] (Block (Data Int32)) -> Maybe (Block (Data Int32))
        find k =
          Map.foldrWithKey (\xs v acc ->
                              if elem k xs then Just v else acc)
                           Nothing

-- cfgLayout :: Script -> Alga.Graph (Maybe Address)
-- cfgLayout src =
--   let blocks = basicBlocks src
--   in Alga.overlays . concatMap mkEdges $ blocks
--   where mkEdges :: Block a -> [Alga.Graph (Maybe Address)]
--         mkEdges (MkBlock a _ Nothing)       = [Alga.edge (Just a) Nothing]
--         mkEdges (MkBlock a _ (Just (l, r))) = [ Alga.edge (Just a) (Just l)
--                                               , Alga.edge (Just a) (Just r) ]

style :: Show a => Alga.Style (Maybe (Block a)) String
style = Alga.Style
    { Alga.graphName               = "CFG"
    , Alga.preamble                = mempty
    , Alga.graphAttributes         = [ "rankdir" := "LR" ]
    , Alga.defaultVertexAttributes = [ "fontsize" := "8" ]
    , Alga.defaultEdgeAttributes   = mempty
    , Alga.vertexAttributes        = \x   ->
        [ "label" Alga.:=
          ( "<<TABLE ALIGN=\"LEFT\">"
          <> table x
          <> "</TABLE>>"
          )
        , "labelloc" Alga.:= "top"
        , "shape" Alga.:= "plain"]
    , Alga.vertexName              = \x   -> maybe "⊥" show (_entry <$> x)
    , Alga.edgeAttributes          = \x y -> [ "headport" := maybe "⊥" (show . show) (_entry <$> y)
                                             , "tailport" := maybe "t" (show . show) (_exit <$> x)]
    }
  where table Nothing = "<TR><TD ALIGN=\"LEFT\">" <> "⊥" <> "</TD></TR>"
        table (Just b) = unlines $ map row (zip [_entry b..] (_body b))
                                     <> [lastRow (_targets $ b)]
          where
            row (a, i) = "<TR><TD ALIGN=\"LEFT\"" <>
                    (" PORT=\"" <> show a <> "\" >") <>
                    show a <> ". " <> show i <>
                    "</TD></TR>"
            lastRow r = "<TR><TD ALIGN=\"CENTER\" BORDER=\"0\"> Targets: "
                     <> show r <> "</TD></TR>"

writeCfgDot :: FilePath -> Script -> IO ()
writeCfgDot path src =
  writeFile path . myExport style  $ cfg src

myExport :: Alga.Style (Maybe (Block a)) String -> Alga.Graph (Maybe (Block a)) -> String
myExport s@Alga.Style {..} g = render $ header <> body <> "}\n"
  where
    header    = "digraph" <+> literal graphName <> "\n{\n"
    with x as = if null as then mempty else line (x <+> myAttributes as)
    line s    = indent 2 s <> "\n"
    body      = Export.unlines (map literal preamble)
             <> ("graph" `with` Alga.graphAttributes s)
             <> ("node"  `with` Alga.defaultVertexAttributes s)
             <> ("edge"  `with` Alga.defaultEdgeAttributes s)
             <> export vDoc eDoc g
    label     = literal . vertexName
    vDoc x    = line $ label x <+>                      myAttributes (vertexAttributes x)
    eDoc x y  = line $ label x <> " -> " <> label y <+> myAttributes (edgeAttributes x y)

myAttributes :: [Alga.Attribute String] -> Doc String
myAttributes [] = mempty
myAttributes as = brackets . mconcat . intersperse " " $ map dot as
  where
    dot (k Alga.:= v) = literal k <> "=" <> literal v
