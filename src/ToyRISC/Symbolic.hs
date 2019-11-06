{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Symbolic
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Typed symbolic expressions syntax
--
-----------------------------------------------------------------------------
module ToyRISC.Symbolic
    (Sym (..), simplify, getValue) where

import           Data.Int      (Int32)
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Prelude       hiding (not)

import           ToyRISC.Types

-----------------------------------------------------------------------------
-- | Symbolic expressions
data Sym a where
    SConst :: a -> Sym a
    SAny   :: Text -> Sym Int32
    SAdd   :: Sym Int32 -> Sym Int32 -> Sym Int32
    SSub   :: Sym Int32 -> Sym Int32 -> Sym Int32
    SMul   :: Sym Int32 -> Sym Int32 -> Sym Int32
    SDiv   :: Sym Int32 -> Sym Int32 -> Sym Int32
    SMod   :: Sym Int32 -> Sym Int32 -> Sym Int32
    SAbs   :: Sym Int32 -> Sym Int32
    SEq    :: Sym Int32 -> Sym Int32 -> Sym Bool
    SGt    :: Sym Int32 -> Sym Int32 -> Sym Bool
    SLt    :: Sym Int32 -> Sym Int32 -> Sym Bool
    SAnd   :: Sym Bool -> Sym Bool -> Sym Bool
    SOr    :: Sym Bool -> Sym Bool -> Sym Bool
    SNot   :: Sym Bool -> Sym Bool

instance Show a => Show (Sym a) where
    show (SAdd x y) = "(" <> show x <> " + " <> show y <> ")"
    show (SSub x y) = "(" <> show x <> " - " <> show y <> ")"
    show (SMul x y) = "(" <> show x <> " * " <> show y <> ")"
    show (SDiv x y) = "(" <> show x <> " / " <> show y <> ")"
    show (SMod x y) = "(" <> show x <> " % " <> show y <> ")"
    show (SAbs x  ) = "|" <> show x <> "|"
    show (SConst x) = show x
    show (SAnd x y) = "(" <> show x <> " & " <> show y <> ")"
    show (SOr  x y) = "(" <> show x <> " | " <> show y <> ")"
    show (SAny n  ) = Text.unpack n
    show (SEq  x y) = "(" <> show x <> " == " <> show y <> ")"
    show (SGt  x y) = "(" <> show x <> " > " <> show y <> ")"
    show (SLt  x y) = "(" <> show x <> " < " <> show y <> ")"
    show (SNot b )  = "¬" <> show b

instance Num (Sym Int32) where
  x + y = SAdd x y
  x * y = SMul x y
  abs x = SAbs x
  signum _ = error "Sym.Num: signum is not defined"
  fromInteger _ = error "Sym.Num: fromInteger is not defined"
  negate _ = error "Sym.Num: negate is not defined"

instance Semigroup (Data (Sym Int32)) where
  (MkData x) <> (MkData y) = MkData (SAdd x y)

instance Monoid (Data (Sym Int32)) where
  mempty = MkData $ SConst 0

instance Boolean (Data (Sym Int32)) where
  true = MkData $ SConst 1
  toBool _ = True
  not x = undefined -- SNot x

instance Boolean (Sym Bool) where
  true = SConst True
  toBool _ = True
  not x = SNot x

instance Boolean (Data (Sym Bool)) where
  true = MkData $ SConst True
  toBool (MkData _) = True
  not (MkData x) = MkData (SNot x)

instance Eq a => Eq (Sym a)  where
  SConst c1  == SConst c2  = c1 == c2
  SAny name1 == SAny name2 = name1 == name2

  SAnd _ _  == SAnd _ _  =
    error "Sym.Eq.(==): can't compare symbolic booleans for equality"
  SOr  _ _  == SOr _ _ =
    error "Sym.Eq.(==): can't compare symbolic booleans for equality"
  SNot _ == SNot _ =
    error "Sym.Eq.(==): can't compare symbolic booleans for equality"
  (SEq  _ _) == (SEq  _ _) =
    error "Sym.Eq.(==): can't compare symbolic booleans for equality"
  (SGt  _ _) == (SGt  _ _) =
    error "Sym.Eq.(==): can't compare symbolic booleans for equality"
  (SLt  _ _) == (SLt  _ _) =
    error "Sym.Eq.(==): can't compare symbolic booleans for equality"

  p@(SAdd _ _) == q@(SAdd _ _) = toBool $ SEq p q
  p@(SSub _ _) == q@(SSub _ _) = toBool $ SEq p q
  p@(SMul _ _) == q@(SMul _ _) = toBool $ SEq p q
  p@(SDiv _ _) == q@(SDiv _ _) = toBool $ SEq p q
  p@(SMod _ _) == q@(SMod _ _) = toBool $ SEq p q
  p@(SAbs   _) == q@(SAbs   _) = toBool $ SEq p q

  _ == _ = False

deriving instance Ord a => Ord (Sym a)

-----------------------------------------------------------------------------
-- | Try to perform constant folding and get the resulting value. Return 'Nothing' on
--   encounter of a symbolic variable.
getValue :: Sym a -> Maybe a
getValue = \case
    (SAny   _) -> Nothing
    (SConst x) -> Just x
    (SAdd p q) -> (+)           <$> getValue p <*> getValue q
    (SSub p q) -> (-)           <$> getValue p <*> getValue q
    (SMul p q) -> (*)           <$> getValue p <*> getValue q
    (SDiv p q) -> (Prelude.div) <$> getValue p <*> getValue q
    (SMod p q) -> (Prelude.mod) <$> getValue p <*> getValue q
    (SAbs x  ) -> Prelude.abs   <$> getValue x
    (SAnd p q) -> (&&)          <$> getValue p <*> getValue q
    (SOr  p q) -> (||)          <$> getValue p <*> getValue q
    (SNot x  ) -> not           <$> getValue x
    (SEq  p q) -> (==)          <$> getValue p <*> getValue q
    (SGt  p q) -> (>)           <$> getValue p <*> getValue q
    (SLt  p q) -> (<)           <$> getValue p <*> getValue q

-- | Constant-fold the expression if it only contains 'SConst' leafs; return the
--   unchanged expression otherwise.
tryFoldConstant :: Sym a -> Sym a
tryFoldConstant x =
  let maybeVal = getValue x
  in case maybeVal of
          Just val -> SConst val
          Nothing  -> x

tryReduce :: Sym a -> Sym a
tryReduce = \case
    SNot x -> SNot (tryReduce x)

    -- 0 + y = y
    (SAdd (SConst 0) y) -> tryReduce y
    -- x + 0 = x
    (SAdd x (SConst 0)) -> tryReduce x
    (SAdd x y) -> tryReduce x `SAdd` tryReduce y
    -- x - 0 = x
    (SSub x (SConst 0)) -> tryReduce x
    (SSub x y) -> tryReduce x `SSub` tryReduce y
    -- T && y = y
    (SAnd (SConst True) y) -> tryReduce y
    -- x && T = x
    (SAnd x (SConst True)) -> tryReduce x
    (SAnd x y            ) -> tryReduce x `SAnd` tryReduce y
    -- F || y = y
    (SOr (SConst False) y) -> tryReduce y
    -- x || F = x
    (SOr x (SConst False)) -> tryReduce x
    (SOr x y) -> tryReduce x `SOr` tryReduce y

    (SEq (SConst 0) (SConst 0)) -> SConst True
    (SEq x y) -> tryReduce x `SEq` tryReduce y
    (SGt (SConst 0) (SConst 0)) -> SConst False
    (SGt x y) -> tryReduce x `SGt` tryReduce y
    (SLt (SConst 0) (SConst 0)) -> SConst False
    (SLt x y) -> tryReduce x `SLt` tryReduce y
    s -> s

simplify :: Int -> Sym a -> Sym a
simplify steps | steps <= 0  = id
               | otherwise   = last . take steps
                             . iterate (tryFoldConstant . tryReduce)
