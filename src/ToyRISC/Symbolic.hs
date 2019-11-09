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
    (Concrete(..), Sym (..), simplify, getValue) where

import           Data.Int      (Int32)
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Data.Typeable
import           Debug.Trace
import           Prelude       hiding (not)

import           ToyRISC.Types

-----------------------------------------------------------------------------

-- | Concrete values: either signed integers or booleans
data Concrete where
  CInt :: Int32 -> Concrete
  CBool :: Bool -> Concrete

deriving instance Eq Concrete
deriving instance Ord Concrete

instance Show Concrete where
  show (CInt i)  = show i
  show (CBool b) = show b

instance Num Concrete where
  (CInt x) + (CInt y) = CInt (x + y)
  x        + y        = error $ "Concrete.Num.+: non-integer arguments " <> show x <> " "
                                                                         <> show y
  (CInt x) * (CInt y) = CInt (x + y)
  x        * y        = error $ "Concrete.Num.*: non-integer arguments " <> show x <> " "
                                                                         <> show y
  abs (CInt x) = CInt (abs x)
  abs x        = error $ "Concrete.Num.abs: non-integer argument " <> show x

  signum (CInt x) = CInt (signum x)
  signum x        = error $ "Concrete.Num.signum: non-integer argument " <> show x

  fromInteger x = CInt (fromInteger x)

  negate (CInt x) = CInt (negate x)
  negate x        = error $ "Concrete.Num.negate: non-integer argument " <> show x

instance Boolean Concrete where
  toBool (CBool b) = b
  toBool x         = error $ "Concrete.Boolean.toBool: non-boolean argument " <> show x
  true = CBool True

  not (CBool b) = CBool (not b)
  not x         = error $ "Concrete.Boolean.not: non-boolean argument " <> show x

  (CBool x) ||| (CBool y) = CBool (x || y)
  x         ||| y         =
    error $ "Concrete.Num.|||: non-boolean arguments " <> show x <> " " <> show y

  (CBool x) &&& (CBool y) = CBool (x && y)
  x         &&& y =
    error $ "Concrete.Num.&&&: non-boolean arguments " <> show x <> " " <> show y

-- | Symbolic expressions
data Sym where
    SConst :: Concrete -> Sym
    SAny   :: Text -> Sym
    SAdd   :: Sym -> Sym -> Sym
    SSub   :: Sym -> Sym -> Sym
    SMul   :: Sym -> Sym -> Sym
    SDiv   :: Sym -> Sym -> Sym
    SMod   :: Sym -> Sym -> Sym
    SAbs   :: Sym -> Sym
    SEq    :: Sym -> Sym -> Sym
    SGt    :: Sym -> Sym -> Sym
    SLt    :: Sym -> Sym -> Sym
    SAnd   :: Sym -> Sym -> Sym
    SOr    :: Sym -> Sym -> Sym
    SNot   :: Sym -> Sym

deriving instance Eq Sym
deriving instance Ord Sym
deriving instance Typeable Sym

instance Show Sym where
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

instance Num Sym where
  x + y = SAdd x y
  x * y = SMul x y
  abs x = SAbs x
  signum _ = error "Sym.Num: signum is not defined"
  fromInteger x = SConst (CInt $ fromInteger x)
  negate _ = error "Sym.Num: negate is not defined"

instance Semigroup (Data Sym) where
  (MkData x) <> (MkData y) = MkData (SAdd x y)

instance Monoid (Data Sym) where
  mempty = MkData $ SConst 0

instance Boolean Sym where
  true = SConst (CBool True)
  toBool x = trace (show x) True
  not x = SNot x

  x ||| y = SOr x y
  x &&& y = SAnd x y

instance Boolean (Data Sym) where
  true = MkData $ SConst (CBool True)
  toBool (MkData x) = trace (show x) True
  not (MkData x) = MkData (SNot x)

  (MkData x) ||| (MkData y) = MkData (SOr x y)
  (MkData x) &&& (MkData y) = MkData (SAnd x y)

instance TryEq Sym where
  x === y = Nontrivial (SEq x y)

instance TryEq (Data Sym) where
  (MkData x) === (MkData y) = Nontrivial (MkData $ SEq x y)

-- instance Eq Sym  where
--   p@(SConst c1)  == q@(SConst c2)  = toBool $ SEq p q -- c1 == c2
--   p@(SAny name1) == q@(SAny name2) = toBool $ SEq p q -- name1 == name2

--   SAnd _ _  == SAnd _ _  =
--     error "Sym.Eq.(==): can't compare symbolic booleans for equality"
--   SOr  _ _  == SOr _ _ =
--     error "Sym.Eq.(==): can't compare symbolic booleans for equality"
--   SNot _ == SNot _ =
--     error "Sym.Eq.(==): can't compare symbolic booleans for equality"
--   (SEq  _ _) == (SEq  _ _) =
--     error "Sym.Eq.(==): can't compare symbolic booleans for equality"
--   (SGt  _ _) == (SGt  _ _) =
--     error "Sym.Eq.(==): can't compare symbolic booleans for equality"
--   (SLt  _ _) == (SLt  _ _) =
--     error "Sym.Eq.(==): can't compare symbolic booleans for equality"

--   p@(SAdd _ _) == q@(SAdd _ _) = toBool $ SEq p q
--   p@(SSub _ _) == q@(SSub _ _) = toBool $ SEq p q
--   p@(SMul _ _) == q@(SMul _ _) = toBool $ SEq p q
--   p@(SDiv _ _) == q@(SDiv _ _) = toBool $ SEq p q
--   p@(SMod _ _) == q@(SMod _ _) = toBool $ SEq p q
--   p@(SAbs   _) == q@(SAbs   _) = toBool $ SEq p q

--   _ == _ = False

-- deriving instance Ord Sym

-----------------------------------------------------------------------------
-- | Try to perform constant folding and get the resulting value. Return 'Nothing' on
--   encounter of a symbolic variable.
getValue :: Sym -> Maybe Concrete
getValue = \case
    (SAny   _) -> Nothing
    (SConst x) -> Just x
    (SAdd p q) -> (+)           <$> getValue p <*> getValue q
    (SSub p q) -> (-)           <$> getValue p <*> getValue q
    (SMul p q) -> (*)           <$> getValue p <*> getValue q
    (SDiv p q) -> error "Sym.getValue: div is undefined"
    -- (Prelude.div) <$> getValue p <*> getValue q
    (SMod p q) -> error "Sym.getValue: mod is undefined"
    -- (Prelude.mod) <$> getValue p <*> getValue q
    (SAbs x  ) -> Prelude.abs   <$> getValue x
    (SAnd p q) -> (&&&)          <$> getValue p <*> getValue q
    (SOr  p q) -> (|||)          <$> getValue p <*> getValue q
    (SNot x  ) -> not           <$> getValue x
    (SEq  p q) -> CBool <$> ((==) <$> getValue p <*> getValue q)
    (SGt  p q) -> CBool <$> ((>)           <$> getValue p <*> getValue q)
    (SLt  p q) -> CBool <$> ((<)           <$> getValue p <*> getValue q)

-- | Constant-fold the expression if it only contains 'SConst' leafs; return the
--   unchanged expression otherwise.
tryFoldConstant :: Sym -> Sym
tryFoldConstant x =
  let maybeVal = getValue x
  in case maybeVal of
          Just val -> SConst val
          Nothing  -> x

tryReduce :: Sym -> Sym
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
    (SAnd (SConst (CBool True)) y) -> tryReduce y
    -- x && T = x
    (SAnd x (SConst (CBool True))) -> tryReduce x
    (SAnd x y            ) -> tryReduce x `SAnd` tryReduce y
    -- F || y = y
    (SOr (SConst (CBool False)) y) -> tryReduce y
    -- x || F = x
    (SOr x (SConst (CBool False))) -> tryReduce x
    (SOr x y) -> tryReduce x `SOr` tryReduce y

    (SEq (SConst (CInt 0)) (SConst (CInt 0))) -> SConst (CBool True)
    (SEq x y) -> tryReduce x `SEq` tryReduce y
    (SGt (SConst (CInt 0)) (SConst (CInt 0))) -> SConst (CBool False)
    (SGt x y) -> tryReduce x `SGt` tryReduce y
    (SLt (SConst 0) (SConst 0)) -> SConst (CBool False)
    (SLt x y) -> tryReduce x `SLt` tryReduce y
    s -> s

simplify :: Int -> Sym -> Sym
simplify steps | steps <= 0  = id
               | otherwise   = last . take steps
                             . iterate (tryFoldConstant . tryReduce)
