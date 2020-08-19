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
    ( Trace(..), mkTrace, writeTraceHtmlFile
    , Node(..), NodeId
    ) where

import qualified Data.Tree                    as Tree
import qualified Data.Tree.View               as TreeView
import           Data.Word                    (Word16)
import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.Instruction.Encode
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
    deriving Functor

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
