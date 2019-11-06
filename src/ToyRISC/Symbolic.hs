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
    (Sym (..)) where

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

-- -- | Form an expression containing an integer constant
-- sConst :: Int32 -> SExpr
-- sConst i = Concrete (CBounded i)

-- -- | Form an expression containing a boolean constant
-- constBool :: Bool -> SExpr
-- constBool b = Concrete (CBool b)

-- sTrue :: SExpr
-- sTrue = constBool True

-- sFalse :: SExpr
-- sFalse = constBool False

-- sOp :: Op -> [SExpr] -> SExpr
-- sOp = Symbolic

-- sAdd :: SExpr -> SExpr -> SExpr
-- sAdd x y = Symbolic Plus [x, y]
-- -----------------------------------------------------------------------------
-- -- | Try to perform constant folding and get the resulting value. Return 'Nothing' on
-- --   encounter of a symbolic variable.
-- getValue :: SExpr -> Maybe CVal
-- getValue = \case
--   Concrete cval -> Just cval
--   Any var -> Nothing
--   expr@(Symbolic op args) ->
--     if leavesAreConcrete expr
--     then undefined
--     else Nothing


-- -- | Check of all leaves of a symbolic expression are, in fact, concrete
-- leavesAreConcrete :: SExpr -> Bool
-- leavesAreConcrete = \case
--   Concrete _       -> True
--   Any _            -> False
--   Symbolic op args ->
--     case (op, args) of
--       (Plus , [x,y]) -> leavesAreConcrete x && leavesAreConcrete y
--       (Times, [x,y]) -> leavesAreConcrete x && leavesAreConcrete y
--       (Minus, [x,y]) -> leavesAreConcrete x && leavesAreConcrete y
--       (Neg  , [x]  ) -> leavesAreConcrete x
--       (Abs  , [x]  ) -> leavesAreConcrete x
--       (Not  , [x]  ) -> leavesAreConcrete x
--       (And  , [x,y]) -> leavesAreConcrete x && leavesAreConcrete y
--       (Or   , [x,y]) -> leavesAreConcrete x && leavesAreConcrete y
--       (Eq   , [x,y]) -> leavesAreConcrete x && leavesAreConcrete y
--       (Lt   , [x,y]) -> leavesAreConcrete x && leavesAreConcrete y
--       (Gt   , [x,y]) -> leavesAreConcrete x && leavesAreConcrete y
