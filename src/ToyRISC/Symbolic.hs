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
-- A simple symbolic execution back-end
-- inspired by [SBV](http://hackage.haskell.org/package/sbv)
-- but more fine-tuned for executing low-level code
--
-----------------------------------------------------------------------------
module ToyRISC.Symbolic
    ( CVal  (..)
    , Kind  (..)
    , Op    (..)
    , SExpr (..)
    -- concrete values smart constructors
    , sConst, constBool, sTrue, sFalse
    -- symbolic operations smart constructors
    , sOp, sAdd
    -- Aux functions
    , smtType, falseCVal, trueCVal
    ) where

import           Data.Int      (Int32)
import           Data.Text

import           ToyRISC.Types

-----------------------------------------------------------------------------
-- | Kind of symbolic value
--   Restricted to booleans, 32-bit signed integers and chars,
--   i.e. unsigned 8-bit words
data Kind = KBool
          | KBounded
          | KChar
          deriving (Eq, Ord)

instance Show Kind where
  show KBool    = "SBool"
  show KBounded = "SInt"
  show KChar    = "SChar"

-- | How the kind maps to SMT land
smtType :: Kind -> String
smtType KBool    = "Bool"
smtType KBounded = "(_ BitVec 32)"
smtType KChar    = "(_ BitVec 8)"
-----------------------------------------------------------------------------
-- | Constant value
data CVal = CBounded !Int32
          -- ^ signed 32-bit integer constant
          | CChar !Char
          -- ^ unsigned 8-bit word constant
          | CBool !Bool
          -- ^ boolean constant
          deriving (Eq, Ord)

instance Show CVal where
  show (CBounded i) = show i
  show (CChar c)    = show c
  show (CBool b)    = show b

-- | Constant False as a 'CVal'. We represent it using the integer value 0.
falseCVal :: CVal
falseCVal = CBounded 0

-- | Constant True as a 'CVal'. We represent it using the integer value 1.
trueCVal :: CVal
trueCVal = CBounded 1
-----------------------------------------------------------------------------
data Op = Plus
        | Times
        | Minus
        | Neg -- ^ negate a signed integer
        | Abs

        | Not -- ^ boolean negation
        | And
        | Or

        | Eq
        | Lt
        | Gt
        deriving (Eq, Ord)

instance Show Op where
  show Plus  = "+"
  show Times = "*"
  show Minus = "-"
  show Neg   = "-"
  show Abs   = "abs"
  show Not   = "not"
  show And   = "&&"
  show Or    = "||"
  show Eq    = "=="
  show Lt    = "<"
  show Gt    = ">"
-----------------------------------------------------------------------------
-- | Symbolic expression (not s-expression!!) is either a concrete value or
--   a named variable, or an application of an operation
--   to a list of arguments (symbolic values)
data SExpr = Concrete !CVal
           | Any !Text
           | Symbolic !Op ![SExpr]

instance Show SExpr where
  show (Concrete c)       = show c
  show (Any name)         = unpack name
  show (Symbolic op args) = '(': show op <> " " <> show args <> " "

instance Eq SExpr where
  Concrete c1 == Concrete c2 = c1 == c2
  Any name1   == Any name2   = name1 == name2
  x           == y           = toBool $ sOp Eq [x,y]

deriving instance Ord SExpr

instance Num SExpr where
  x + y = sOp Plus [x,y]
  x * y = sOp Times [x,y]
  abs x = sOp Abs [x]
  signum _ = error "SExpr.Num: signum is not defined"
  fromInteger _ = error "SExpr.Num: fromInteger is not defined"
  negate x = sOp Neg [x]

instance Semigroup (Data SExpr) where
  (MkData x) <> (MkData y) = MkData (sOp Plus [x, y])

instance Monoid (Data SExpr) where
  mempty = MkData $ sConst 0

instance Boolean SExpr where
  true = sTrue
  toBool _ = True
  not x = sOp Not [x]

instance Boolean (Data SExpr) where
  true = MkData sTrue
  toBool (MkData _) = True
  not (MkData x) = MkData (sOp Not [x])

-- | Form an expression containing an integer constant
sConst :: Int32 -> SExpr
sConst i = Concrete (CBounded i)

-- | Form an expression containing a boolean constant
constBool :: Bool -> SExpr
constBool b = Concrete (CBool b)

sTrue :: SExpr
sTrue = constBool True

sFalse :: SExpr
sFalse = constBool False

sOp :: Op -> [SExpr] -> SExpr
sOp = Symbolic

sAdd :: SExpr -> SExpr -> SExpr
sAdd x y = Symbolic Plus [x, y]
