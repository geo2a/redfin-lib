{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
 Module     : ISA.Backend.Graph
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Calculate a control-flow graph of a program
-}
module ISA.Backend.Graph (cfg, writeCfgDot) where

import Algebra.Graph.Export hiding (unlines)
import qualified Algebra.Graph.Export as Export
import Algebra.Graph.Export.Dot (Attribute (..), Style (..))
import qualified Algebra.Graph.Export.Dot as Alga
import qualified Algebra.Graph.Labelled as Alga
import Data.Int
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text.Lazy as Text
import Text.Pretty.Simple

import ISA.Assembly (Script)
import ISA.Backend.Graph.BasicBlock
import ISA.Types.Context
import ISA.Types.Prop
import ISA.Types.ZeroOneTwo

cfg :: Maybe (Context Int32) -> Script -> Alga.Graph Int32 (Maybe (Block Int32))
cfg init src =
    let blocks = basicBlocks init src
        ids = map (\(MkBlock a is _ _) -> map fst $ zip [a ..] is) blocks
        dict = Map.fromList $ zip ids blocks
     in Alga.overlays . concatMap (mkEdges dict) $ blocks
  where
    mkEdges _ b@(MkBlock _ _ _ Zero) = [Alga.edge true (Just b) Nothing]
    mkEdges dict b@(MkBlock _ _ _ (One (n, _))) =
        [Alga.edge true (Just b) (find n dict)]
    mkEdges dict b@(MkBlock _ _ _ (Two (l, _) (r, _))) =
        [ Alga.edge true (Just b) (find l dict)
        , Alga.edge true (Just b) (find r dict)
        ]
    find k =
        Map.foldrWithKey
            ( \xs v acc ->
                if elem k xs then Just v else acc
            )
            Nothing

style :: Show a => Alga.Style (Maybe (Block a)) Text
style =
    Alga.Style
        { Alga.graphName = "CFG"
        , Alga.preamble = mempty
        , Alga.graphAttributes = ["rankdir" := "LR"]
        , Alga.defaultVertexAttributes = ["fontsize" := "8"]
        , Alga.defaultEdgeAttributes = []
        , Alga.vertexAttributes = \x ->
            [ "label"
                Alga.:= ( "<<TABLE ALIGN=\"LEFT\">"
                            <> table x
                            <> "</TABLE>>"
                        )
            , "labelloc" Alga.:= "top"
            , "shape" Alga.:= "plain"
            ]
        , Alga.vertexName = \x -> maybe "⊥" (Text.pack . show) (_entry <$> x)
        , Alga.edgeAttributes =
            \x y ->
                [ "headport" := maybe "⊥" (Text.pack . show) (_entry <$> y)
                , "tailport" := maybe "t" (Text.pack . show) (_exit <$> x)
                ]
                    <> ( case _targets <$> x of
                            Just (Two (a1, c1) (_, c2)) ->
                                [ "label"
                                    := (\s -> "\"" <> s <> "\"")
                                        ( if Just a1 == (_entry <$> y)
                                            then pShowNoColor c1
                                            else pShowNoColor c2
                                        )
                                , "decorate" := "TRUE"
                                , "fontsize" := "8"
                                ]
                            _ -> []
                       )
        }
  where
    table Nothing = "<TR><TD ALIGN=\"LEFT\">" <> "⊥" <> "</TD></TR>"
    table (Just b) =
        Text.unlines $
            map row (zip [_entry b ..] (_body b))
                <> [lastRow (_targets $ b)]
      where
        row (a, i) =
            "<TR><TD ALIGN=\"LEFT\""
                <> (" PORT=\"" <> (Text.pack . show) a <> "\" >")
                <> (Text.pack . show) a
                <> ". "
                <> (Text.pack . show) i
                <> "</TD></TR>"
        lastRow r =
            "<TR><TD ALIGN=\"CENTER\" BORDER=\"0\"> Targets: "
                <> (Text.pack . show . (fst <$>)) r
                <> "</TD></TR>"

writeCfgDot :: FilePath -> Maybe (Context Int32) -> Script -> IO ()
writeCfgDot path init src =
    writeFile path . unpack . myExport style $ cfg init src

myExport ::
    ( Monoid a
    , Integral a
    ) =>
    Alga.Style (Maybe (Block a)) Text ->
    Alga.Graph a (Maybe (Block a)) ->
    Text
myExport s@Alga.Style{..} g = render $ header <> body <> "}\n"
  where
    header = "digraph" <+> literal graphName <> "\n{\n"
    with x as = if null as then mempty else line (x <+> myAttributes as)
    line s = indent 2 s <> "\n"
    body =
        Export.unlines (map literal preamble)
            <> ("graph" `with` Alga.graphAttributes s)
            <> ("node" `with` Alga.defaultVertexAttributes s)
            <> ("edge" `with` Alga.defaultEdgeAttributes s)
            <> export vDoc eDoc g
    label = literal . vertexName
    vDoc x = line $ label x <+> myAttributes (vertexAttributes x)
    eDoc x y = line $ label x <> " -> " <> label y <+> myAttributes (edgeAttributes x y)

myAttributes :: [Alga.Attribute Text] -> Doc Text
myAttributes [] = mempty
myAttributes as = brackets . mconcat . intersperse " " $ map dot as
  where
    dot (k Alga.:= v) = literal k <> "=" <> literal v
