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

import Control.Selective
import Data.Kind (Constraint, Type)
import Prelude hiding (Monad, (>>=))
import qualified Prelude (Monad, (>>=))

-- | Trivial class with no restrictions -- any type of kind * has an instance
class Any a

instance Any a

class (Selective f, Prelude.Monad f) => Monad f where
    (>>=) :: f a -> (a -> f b) -> f b

instance (Selective f, Prelude.Monad f) => Monad f where
    (>>=) = (Prelude.>>=)

-- | Constrain the type 'a' by a list of constraints
type family CS (cs :: [Type -> Constraint]) (a :: Type) :: Constraint where
    CS cs a = CSGo cs (Any a) a

type family CSGo (cs :: [Type -> Constraint]) (acc :: Constraint) a :: Constraint where
    CSGo '[] acc _ = acc
    CSGo (x ': xs) acc a = CSGo xs (x a, acc) a

type FS
    (key :: Type)
    (control :: (Type -> Type) -> Constraint)
    (value :: [Type -> Constraint])
    (a :: Type) =
    forall f.
    (control f, CS value a) =>
    (key -> f a) ->
    (key -> f a -> f a) ->
    f a
