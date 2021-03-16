{-# LANGUAGE OverloadedStrings   #-}
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

import           Algebra.Graph.Export         hiding (unlines)
import qualified Algebra.Graph.Export         as Export
import           Algebra.Graph.Export.Dot     (Attribute (..), Style (..))
import qualified Algebra.Graph.Export.Dot     as Alga
import qualified Algebra.Graph.Labelled       as Alga
import           Control.Selective
import           Data.Bifunctor
import           Data.Int
import           Data.List                    (intersperse)
import qualified Data.Map                     as Map
import           Data.String                  hiding (unlines)
import           Data.Text.Lazy               (Text, unpack)
import qualified Data.Text.Lazy               as Text
import qualified Debug.Trace                  as Debug
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Text.Pretty.Simple

import           ISA.Assembly                 (Script, assemble)
import           ISA.Backend.Graph.BasicBlock
import           ISA.Backend.Simulation
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Prop
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address   hiding (literal)
import           ISA.Types.ZeroOneTwo

cfg :: Maybe (Context (Data Int32)) -> Script -> Alga.Graph (Data Int32) (Maybe (Block (Data Int32)))
cfg init src =
  let blocks = basicBlocks init src
      ids = map (\(MkBlock a is _ _) -> map fst $ zip [a..] is) blocks
      dict = Map.fromList $ zip ids blocks
  in Alga.overlays . concatMap (mkEdges dict) $ blocks
  where mkEdges dict b@(MkBlock a _ _ Zero)       = [Alga.edge true (Just b) Nothing]
        mkEdges dict b@(MkBlock a _ _ (One (n, _))) =
          [ Alga.edge true (Just b) (find n dict) ]
        mkEdges dict b@(MkBlock a _ _ (Two (l, cl) (r, cr))) =
          [ Alga.edge true (Just b) (find l dict)
          , Alga.edge true (Just b) (find r dict) ]
--        find :: Address -> Map.Map [Address] (Block (Data Int32)) -> Maybe (Block (Data Int32))
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

style :: Show a => Alga.Style (Maybe (Block a)) Text
style = Alga.Style
    { Alga.graphName               = "CFG"
    , Alga.preamble                = mempty
    , Alga.graphAttributes         = [ "rankdir" := "LR" ]
    , Alga.defaultVertexAttributes = [ "fontsize" := "8" ]
    , Alga.defaultEdgeAttributes   = []
    , Alga.vertexAttributes        = \x   ->
        [ "label" Alga.:=
          ( "<<TABLE ALIGN=\"LEFT\">"
          <> table x
          <> "</TABLE>>"
          )
        , "labelloc" Alga.:= "top"
        , "shape" Alga.:= "plain"]
    , Alga.vertexName              = \x   -> maybe "⊥" (Text.pack . show) (_entry <$> x)
    , Alga.edgeAttributes          =
      \x y -> [ "headport" := maybe "⊥" (Text.pack . show) (_entry <$> y)
              , "tailport" := maybe "t" (Text.pack . show) (_exit <$> x)
              ] <>
              (case _targets <$> x of
                 Just (Two (a1, c1) (a2, c2)) ->
                   [ "label" := (\s -> "\"" <> s <> "\"")
                       (if Just a1 == (_entry <$> y) then pShowNoColor c1 else pShowNoColor c2)
                   , "decorate" := "TRUE"
                   , "fontsize" := "8"
                   ]
                 _ -> [])
    }
  where table Nothing = "<TR><TD ALIGN=\"LEFT\">" <> "⊥" <> "</TD></TR>"
        table (Just b) = Text.unlines $ map row (zip [_entry b..] (_body b))
                                     <> [lastRow (_targets $ b)]
          where
            row (a, i) = "<TR><TD ALIGN=\"LEFT\"" <>
                    (" PORT=\"" <> (Text.pack . show) a <> "\" >") <>
                    (Text.pack . show) a <> ". " <> (Text.pack . show) i <>
                    "</TD></TR>"
            lastRow r = "<TR><TD ALIGN=\"CENTER\" BORDER=\"0\"> Targets: "
                     <> (Text.pack . show . (fst <$>)) r <> "</TD></TR>"

writeCfgDot :: FilePath -> Maybe (Context (Data Int32)) -> Script -> IO ()
writeCfgDot path init src =
  writeFile path . unpack . myExport style $ cfg init src

myExport ::
  ( Addressable a, Num a, Show a
  , Monoid a, Integral a, Bounded a, Boolean a, TryOrd a)
  => Alga.Style (Maybe (Block a)) Text -> Alga.Graph a (Maybe (Block a)) -> Text
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

myAttributes :: [Alga.Attribute Text] -> Doc Text
myAttributes [] = mempty
myAttributes as = brackets . mconcat . intersperse " " $ map dot as
  where
    dot (k Alga.:= v) = literal k <> "=" <> literal v
