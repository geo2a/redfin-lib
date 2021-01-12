{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.Trace
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Tree-shaped symbolic execution trace
--
-----------------------------------------------------------------------------
module ISA.Types.Symbolic.Trace
    ( Trace(..), mkTrace, subsetTrace, traceDepth, htmlTrace, writeTraceHtmlFile
    , Path, paths, constrainTrace
    , Node(..), NodeId, lookup
    ) where

import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Tree                  as Tree
import qualified Data.Tree.View             as TreeView
import           Prelude                    hiding (lookup)

import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context

type NodeId = Int

data Node s = Node { nodeId   :: NodeId
                   , nodeBody :: s
                   } deriving Functor

instance Eq (Node s) where
    (Node x _) == (Node y _) = x == y

instance Ord (Node s) where
    (Node x _) <= (Node y _) = x <= y

instance Show (Node s) where
    show (Node nId _) = show nId

-- | Symbolic execution trace
newtype Trace s = Trace {unTrace :: Tree.Tree (Node s)}
    deriving (Show, Functor)

instance Foldable Trace where
    foldMap f (Trace tree) = foldMap (f . nodeBody) tree

instance Traversable Trace where
    traverse f (Trace tree) = Trace <$> traverse (\(Node n s) -> Node n <$> f s) tree

-- | Render a trace as an HTML string
htmlTrace :: (Context -> String) -> Trace Context -> String
htmlTrace shower (Trace tree) =
  TreeView.htmlTree Nothing $
    fmap (\node -> TreeView.NodeInfo
                   TreeView.InitiallyExpanded (show (nodeId node) <> " | " <>
                                               showIR (nodeBody node))
                                               (shower (nodeBody node)))
         tree

-- | Render a trace as HTML and write the result into a file
writeTraceHtmlFile :: (Context -> String) -> FilePath -> Trace Context -> IO ()
writeTraceHtmlFile shower path trace = writeFile path (htmlTrace shower trace)

mkTrace :: Node Context -> [Trace Context] -> Trace Context
mkTrace node children = Trace $ Tree.Node node (map unTrace children)

constrainTrace :: (Text, Context -> Sym) -> Trace Context -> Trace Context
constrainTrace (name, expr) =
  fmap (\ctx -> ctx {_constraints = (name, expr ctx):_constraints ctx})

-- -- | Impose a path constraint on every state in the trace.
-- --   Useful for checking whole program properties, e.g. the absence of overflow
-- constraint :: Label -> (State -> Sym Bool) -> Trace State -> Trace State
-- constraint label constr = fmap (\s -> appendConstraints [(label, constr s)] s)

traceDepth :: Trace s -> Int
traceDepth = length . Tree.flatten . unTrace

subsetTrace :: (s -> Bool) -> Trace s -> [Node s]
subsetTrace property (Trace tree) =
    foldMap (\s -> if property (nodeBody s) then [s] else []) tree

type Path s = [s]

-- | Enumerate all paths in a rose tree
paths :: Tree.Tree a -> [Path a]
paths = \case
    (Tree.Node payload []) -> [[payload]]
    (Tree.Node payload xs) -> concat [map (payload:) (paths t) | t <- xs]

lookup :: NodeId -> Trace a -> Maybe a
lookup n (Trace t) =
  Tree.foldTree (\x xs -> if nodeId x == n then Just (nodeBody x)
                          else listToMaybe (catMaybes xs)) t
