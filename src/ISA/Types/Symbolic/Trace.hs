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
import           Data.Traversable           (forM)
import qualified Data.Tree                  as Tree
import qualified Data.Tree.View             as TreeView
import           Prelude                    hiding (lookup)

import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT

type NodeId = Int

data Node s = Node { _nodeId   :: NodeId
                   , _nodeBody :: s
                   } deriving Functor

instance Eq (Node s) where
  x == y = _nodeId x == _nodeId y

instance Ord (Node s) where
  x <= y = _nodeId x <= _nodeId y

instance Show (Node s) where
  show node = show (_nodeId node)

-- | Symbolic execution trace
newtype Trace s = Trace {unTrace :: Tree.Tree (Node s)}
  deriving (Show, Functor)

instance Foldable Trace where
  foldMap f (Trace tree) = foldMap (f . _nodeBody) tree

instance Traversable Trace where
  traverse f (Trace tree) =
    Trace <$> traverse (\(Node n b) -> Node n <$> f b) tree

-- | Render a trace as an HTML string
htmlTrace :: (Context -> String) -> Trace Context -> String
htmlTrace shower (Trace tree) =
  TreeView.htmlTree Nothing $
    fmap (\node -> TreeView.NodeInfo
                   TreeView.InitiallyExpanded (show (_nodeId node) <> " | " <>
                                               showIR (_nodeBody node))
                                               (shower (_nodeBody node)))
         tree

-- | Render a trace as HTML and write the result into a file
writeTraceHtmlFile :: (Context -> String) -> FilePath -> Trace Context -> IO ()
writeTraceHtmlFile shower path trace = writeFile path (htmlTrace shower trace)

mkTrace :: Node Context -> [Trace Context] -> Trace Context
mkTrace node children = Trace $ Tree.Node node (map unTrace children)

constrainTrace :: (Text, Context -> Sym) -> Trace Context -> Trace Context
constrainTrace (name, expr) =
  fmap (\ctx -> ctx {_constraints = (name, expr ctx):_constraints ctx})

traceDepth :: Trace s -> Int
traceDepth = length . Tree.flatten . unTrace

subsetTrace :: (s -> Bool) -> Trace s -> [Node s]
subsetTrace property (Trace tree) =
    foldMap (\s -> if property (_nodeBody s) then [s] else []) tree

type Path s = [s]

-- | Enumerate all paths in a rose tree
paths :: Tree.Tree a -> [Path a]
paths = \case
    (Tree.Node payload []) -> [[payload]]
    (Tree.Node payload xs) -> concat [map (payload:) (paths t) | t <- xs]

lookup :: NodeId -> Trace a -> Maybe a
lookup n (Trace t) =
  Tree.foldTree (\x xs -> if _nodeId x == n then Just (_nodeBody x)
                          else listToMaybe (catMaybes xs)) t

-- solveTrace :: Trace Context -> IO (Trace Context)
-- solveTrace (Trace tr) = Trace <$> (forM tr $ \node -> do
--   ctx <- solveContext (_nodeBody node)
--   pure $ node { _nodeBody = ctx })
