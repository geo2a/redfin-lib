{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}

-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.List.Trace
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Rose-tree shaped symbolic execution trace
--
-----------------------------------------------------------------------------
module ISA.Backend.Symbolic.List.Trace
    ( Trace(..), mkTrace
    , initial, leafs, nodes
    , subsetTrace, traceDepth
    , Path, paths, constrainTrace
    , Node(..), NodeId, lookup
    ) where

import           Data.Aeson             (FromJSON, ToJSON, defaultOptions,
                                         genericToEncoding, toEncoding)
import           Data.Maybe             (catMaybes, listToMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import           Data.Traversable       (forM)
import           Data.Tree              (Tree)
import qualified Data.Tree              as Tree
import qualified Data.Tree.View         as TreeView
import           GHC.Generics
import           Prelude                hiding (lookup)

import           ISA.Types.Context      hiding (Context)
import qualified ISA.Types.Context      as ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.SMT

type Context = ISA.Types.Context Sym

type NodeId = Int

data Node s = Node { _nodeId   :: NodeId
                   , _nodeBody :: s
                   } deriving (Functor, Generic, ToJSON, FromJSON)

instance Eq (Node s) where
  x == y = _nodeId x == _nodeId y

instance Ord (Node s) where
  x <= y = _nodeId x <= _nodeId y

instance Show (Node s) where
  show node = show (_nodeId node)

-- | Symbolic execution trace
newtype Trace s = Trace {unTrace :: Tree.Tree (Node s)}
  deriving (Functor, Generic, ToJSON, FromJSON)

-- | The root of a trace
initial :: Trace s -> Node s
initial (Trace tree) = Tree.rootLabel tree

instance Show a => Show (Trace a) where
  show (Trace tree) = Tree.drawTree (fmap (show . _nodeBody) tree)

instance Foldable Trace where
  foldMap f (Trace tree) = foldMap (f . _nodeBody) tree

instance Traversable Trace where
  traverse f (Trace tree) =
    Trace <$> traverse (\(Node n b) -> Node n <$> f b) tree

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

nodes :: Trace s -> [Node s]
nodes = subsetTrace (const True)

leafs :: Trace s -> [Node s]
leafs (Trace tree) =
  case Tree.levels tree of
    [] -> []
    xs -> last xs

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
