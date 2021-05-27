{-# LANGUAGE QuantifiedConstraints #-}

{- |
 Module     : ISA.Types.Boolean
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Boolean, equality and order type classes
-}
module ISA.Types.Boolean (
    -- ** Booleans
    Boolean (..),

    -- ** equality and order checks that may fail
    BEq (..),
    BOrd (..),
) where

import Prelude hiding (not)
import qualified Prelude

class Boolean a where
    toBool :: a -> Bool
    fromBool :: Bool -> a
    true :: a
    false :: a
    false = not true
    not :: a -> a

    infixr 2 |||
    (|||) :: a -> a -> a

    infixr 3 &&&
    (&&&) :: a -> a -> a

instance Boolean Bool where
    true = True
    not = Prelude.not
    toBool = id
    fromBool = id

    x ||| y = x || y
    x &&& y = x && y

class BEq a where
    infix 4 ===
    (===) :: a -> a -> a

class BEq a => BOrd a where
    infix 4 `lt`
    lt :: a -> a -> a
    infix 4 `gt`
    gt :: a -> a -> a
