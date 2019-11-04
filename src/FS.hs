{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
-----------------------------------------------------------------------------
-- |
-- Module     : FS
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Fine-grained state
-----------------------------------------------------------------------------
module FS (FS) where

import           Data.Constraint (Constraint)

type FS (key :: *)
        (control :: (* -> *) -> Constraint)
        (value :: * -> Constraint)
        (a :: *) =
  forall f . (control f, value a)
          => (key -> f a)
          -> (key -> f a -> f a)
          -> f a
