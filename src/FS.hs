{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
 Module     : FS
 Copyright  : (c) Georgy Lukyanov 2019
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Fine-grained state
-}
module FS where

import Data.Constraint

-- | Trivial class with no restrictions -- any type of kind * has an instance
class Any a

instance Any a

-- | Constrain the type 'a' by a list of constraints
type family CS (cs :: [* -> Constraint]) (a :: *) :: Constraint where
    CS cs a = CSGo cs (Any a) a

type family CSGo (cs :: [* -> Constraint]) (acc :: Constraint) a :: Constraint where
    CSGo '[] acc _ = acc
    CSGo (x ': xs) acc a = CSGo xs (x a, acc) a

type FS
    (key :: *)
    (control :: (* -> *) -> Constraint)
    (value :: [* -> Constraint])
    (a :: *) =
    forall f.
    (control f, CS value a) =>
    (key -> f a) ->
    (key -> f a -> f a) ->
    f a
