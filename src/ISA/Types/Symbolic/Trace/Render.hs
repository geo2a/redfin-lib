{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.Trace.Render
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Render symbolic execution traces into HTML
--
-----------------------------------------------------------------------------

module ISA.Types.Symbolic.Trace.Render
    (writeTraceHtmlFile) where

import           Control.Monad                   (forM_)
import qualified Data.Map                        as Map
import           Data.String                     (fromString)
import qualified Data.Tree                       as Tree
import           Prelude                         hiding (div, head, id, span)
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes     (class_, for, href, id, rel,
                                                  type_)

import           ISA.Backend.Symbolic.List
import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Trace        hiding (renderTrace)

renderContext :: Context -> Html
renderContext ctx =
  div ! class_ "context" $ do
    div ! class_ "context-path-condition" $
      toHtml $ show (_pathCondition ctx)
    div ! class_ "context-bindings-list"  $
      Map.foldMapWithKey (\k v -> (renderBinding k v)) (_bindings ctx)

renderBinding :: Key -> Sym -> Html
renderBinding key value =
  let htmlClass = fromString $ "binding-" <> keyTag key
  in case key of
       IR ->
         case toInstruction value of
           Left _ -> div ! class_ htmlClass $ "<symbolic>"
           Right instr ->
             div ! class_ htmlClass $ toHtml $ show key <> " ↦ " <> show instr
       Prog _ -> mempty
       _ -> div ! class_ htmlClass $ toHtml $ show key <> " ↦ " <> show value

renderNode :: Node Context -> Html
renderNode node =
  let buttonId = fromString $ "tooltip-button-" <> (show $ nodeId node)
  in  label ! class_ "tooltip" ! for buttonId $ do
        toHtml (show $ nodeId node)
        input ! type_ "checkbox" ! id buttonId
        div ! class_ "tooltiptext" $ renderContext (nodeBody node)

renderTrace :: Trace Context -> Html
renderTrace (Trace trace) =
  docTypeHtml $ do
    head $ do
      title "Symbolic execution trace"
      link ! rel "stylesheet" ! type_ "text/css" ! href "trace.css"
    body $
      div ! class_ "tree" $
        ul ! class_ "trace-level-0" $
          li $ renderTree 0 trace
  where
    renderTree :: Int -> Tree.Tree (Node Context) -> Html
    renderTree level (Tree.Node x ts) = case ts of
      -- []       -> div ! class_ (fromString $ "trace-level-" <> show level) $ renderNode x
      subtrees -> do
        renderNode x
        ul ! class_ (fromString $ "trace-level-" <> show (level + 1)) $
          forM_ subtrees (li . renderTree (level + 1))

writeTraceHtmlFile :: FilePath -> Trace Context -> IO ()
writeTraceHtmlFile fname trace =
  writeFile fname (renderHtml (renderTrace trace))

-- numbers :: Int -> Html
-- numbers n = docTypeHtml $ do
--     HTML.head $ do
--         HTML.title "Natural numbers"
--     body $ do
--         p "A list of natural numbers:"
--         ul $ forM_ [1 .. n] (li . toHtml)
