{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ISA.Types.Tree
  ( Tree(..), insert1, insert2, leafs, draw
  , Cxt(..), Loc(..), locKey
  , Travel(..), travel, travelVerbose, shift
  , left, up, right, down, top
  , getTree, putTree, findLoc
  )where

import           Control.Applicative
import           Control.Monad.State

data Tree key a = Leaf key a
                | Trunk key (Tree key a)
                | Branch key (Tree key a) (Tree key a)
                deriving (Show, Functor)

rootKey :: Tree key a -> key
rootKey = \case Leaf k _     -> k
                Trunk k _    -> k
                Branch k _ _ -> k

insert1 :: (Enum key, Eq key) => Tree key a -> key -> a -> Tree key a
insert1 tree nid y =
  case tree of
    Leaf n x -> if n == nid then Trunk n (Leaf (succ nid) y) else Leaf n x
    Trunk n x -> Trunk n (insert1 x nid y)
    (Branch n l0 r0) -> Branch n (insert1 l0 nid y) (insert1 r0 nid y)

insert2 :: (Enum key, Eq key) => Tree key a -> key -> a -> a -> Tree key a
insert2 tree nid l r =
  case tree of
    Leaf n x -> if n == nid then Branch n (Leaf (succ nid) l) (Leaf (succ (succ nid)) r) else Leaf n x
    Trunk n x -> Trunk n (insert2 x nid l r)
    (Branch n l0 r0) -> Branch n (insert2 l0 nid l r) (insert2 r0 nid l r)

leafs :: Tree key a -> [(key, a)]
leafs = go []
  where
    go acc = \case
      Leaf k v     -> (k, v):acc
      Trunk _ t    -> go acc t
      Branch _ l r -> go acc l ++ go acc r

findLoc :: Eq key => key -> Tree key a -> Maybe (Loc key a)
findLoc k tree =
  case go (pure ()) tree of
    Nothing   -> Nothing
    Just path -> Just $ shift (Loc tree Top) path
  where
    go acc = \case
      Branch m l r -> if k == m then Just acc else go (acc >> left) l <|> go (acc >> right) r
      Trunk m c -> if k == m then Just acc else go (acc >> down) c
      Leaf m _ -> if k == m then Just acc else Nothing

draw :: (Show key, Show a) => Tree key a -> [String]
draw = \case
  (Leaf x v)     -> lines (show (x, v))
  (Trunk x t)    -> lines (show x) ++ drawSubTrees [t]
  (Branch x l r) -> lines (show x) ++ drawSubTrees [l, r]
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

--------------------------------------------------------------------------------
--- Zipper on binary trees -----------------------------------------------------
--------------------------------------------------------------------------------

data Cxt key a = Top
               | D key (Cxt key a)
               | L key (Cxt key a) (Tree key a)
               | R key (Tree key a) (Cxt key a)
               deriving Show

data Loc key a = Loc { struct :: Tree key a
                     , cxt    :: Cxt key a }
  deriving Show

locKey :: Loc key a -> key
locKey (Loc k _) = rootKey k

newtype Travel loc a = Travel { unT :: State loc a }
     deriving (Functor, Applicative, Monad, MonadState loc)

-- | Execute the zipper scrip form a starting location on a tree
travel :: Loc key a
       -> Travel (Loc key a) b
       -> b
travel start tt = evalState (unT tt) start

shift :: Loc key a -> (Travel (Loc key a) ()) -> Loc key a
shift from tt = execState (unT tt) from

travelVerbose :: Loc key a -> Travel (Loc key a) b -> (b, Loc key a)
travelVerbose start tt = runState (unT tt) start

top :: Travel (Loc key a) ()
top = do
  l@(Loc _ cxt) <- get
  case cxt of
    Top -> put l
    _   -> up *> top

left :: Travel (Loc key a) ()
left = do
  (Loc tree cxt) <- get
  case tree of
     Branch n lChild rChild -> put $ Loc lChild (L n cxt rChild)
     _                      -> put $ Loc tree cxt

right :: Travel (Loc key a) ()
right = do
  (Loc tree cxt) <- get
  case tree of
     Branch n lChild rChild -> put $ Loc rChild (R n lChild cxt)
     _                      -> put $ Loc tree cxt

up :: Travel (Loc key a) ()
up = do
  (Loc t cxt) <- get
  case cxt of
    D n c   -> put $ Loc (Trunk n t) c
    L n c r -> put $ Loc (Branch n t r) c
    R n l c -> put $ Loc (Branch n l t) c
    _       -> put (Loc t cxt)

down :: Travel (Loc key a) ()
down = do
  (Loc t cxt) <- get
  case t of
    Trunk n child -> put $ Loc child (D n cxt)
    _             -> put $ Loc t cxt

modifyTree :: (Tree key a -> Tree key a) -> Travel (Loc key a) (Tree key a)
modifyTree f = modify editStruct >> liftM struct get
  where editStruct (Loc s c) = Loc (f s) c

putTree :: Tree key a -> Travel (Loc key a) ()
putTree t = do
  (Loc _ cxt) <- get
  put(Loc t cxt)

getTree :: Travel (Loc key a) (Tree key a)
getTree = do
  (Loc t _) <- get
  pure t
