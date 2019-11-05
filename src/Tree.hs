{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Tree
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- A simple binary tree datatype
--
-----------------------------------------------------------------------------

module Tree
    ( Tree(..), empty, mkLeaf, insert, fromList
    , drawTree
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text

data Tree a where
  Empty :: Tree a
  Node  :: a -> Tree a -> Tree a -> Tree a

deriving instance Functor Tree

-- | Create an empty tree
empty :: Tree a
empty = Empty

mkLeaf :: a -> Tree a
mkLeaf val = Node val Empty Empty

-- | Insert a node into a tree
insert :: Ord a => a -> Tree a -> Tree a
insert x = \case
  Empty -> mkLeaf x
  Node y l r | x == y     -> Node x l r
             | x < y      -> Node y (insert x l) r
             | otherwise  -> Node y l (insert x r)

fromList :: Ord a => [a] -> Tree a
fromList = \case
  []     -> empty
  [x]    -> mkLeaf x
  (x:xs) -> insert x (fromList xs)

-----------------------------------------------------------------------------
drawTree :: Show a => Tree a -> String
drawTree = Text.unpack . Text.unlines . draw . fmap (Text.pack . show)

draw :: Tree Text -> [Text]
draw = \case
  Node x l r -> Text.lines x <> drawSubTrees [l, r]
  Empty      -> drawSubTrees []
  where
    drawSubTrees [] = ["*"]
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) <> drawSubTrees ts

    shift first other = zipWith (<>) (first : repeat other)
