{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Instruction
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Syntax of ISA instructions
-----------------------------------------------------------------------------
module ISA.Types.Instruction
    ( Instruction(..), mkI
    , InstructionImpl(..), instrEq
    )
  where

import           Control.Selective
import           Data.Int          (Int16, Int64, Int8)
import           Data.Kind         (Constraint)
import           Data.Word         (Word16, Word64, Word8)
import           Prelude           hiding (Read, readIO)
import           Prelude           hiding (Monad, Read, abs, div, mod)
import qualified Prelude           (Monad, Read, abs, div, mod)
import GHC.Generics (Generic)

import           ISA.Types

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad (m :: * -> *) = (Selective m, Prelude.Monad m)

-----------------------------------------------------------------------------

--------------------------------------------------------------------------------
---------------- Instructions --------------------------------------------------
--------------------------------------------------------------------------------

class Unconstrained (a :: * -> *)
instance Unconstrained a

data InstructionImpl (c :: (* -> *) -> Constraint) a where
  Halt     :: InstructionImpl Applicative a
  Load     :: Register -> Address -> InstructionImpl Functor a
  Set      :: Value a => Register -> Imm a         -> InstructionImpl Functor a
  Store    :: Register -> Address -> InstructionImpl Functor a
  Add      :: Register -> Address -> InstructionImpl Applicative a
  Sub      :: Register -> Address -> InstructionImpl Applicative a
  Mul      :: Register -> Address -> InstructionImpl Applicative a
  Div      :: Register -> Address -> InstructionImpl Applicative a
  Mod      :: Register -> Address -> InstructionImpl Applicative a
  Abs      :: Register                  -> InstructionImpl Applicative a
  Jump     :: Value a => Imm a                    -> InstructionImpl Applicative a
  LoadMI   :: Register -> Address -> InstructionImpl Prelude.Monad a

  CmpEq    :: Register  -> Address -> InstructionImpl Selective a
  CmpGt    :: Register  -> Address -> InstructionImpl Selective a
  CmpLt    :: Register  -> Address -> InstructionImpl Selective a

  JumpCt   :: Value a => Imm a -> InstructionImpl Selective a
  JumpCf   :: Value a => Imm a -> InstructionImpl Selective a

deriving instance Eq a => Eq (InstructionImpl c a)
-- deriving instance Ord (InstructionImpl c)

instance Show (InstructionImpl c a) where
  show = \case
    Halt               -> "Halt"
    Load     reg addr  -> "Load "     ++ show reg ++ " " ++ show addr
    Set      reg value -> "Set "      ++ show reg ++ " " ++ show value
    Store    reg addr  -> "Store "    ++ show reg ++ " " ++ show addr
    Add      reg addr  -> "Add "      ++ show reg ++ " " ++ show addr
    Sub      reg addr  -> "Sub "      ++ show reg ++ " " ++ show addr
    Mul      reg addr  -> "Mul "      ++ show reg ++ " " ++ show addr
    Div      reg addr  -> "Div "      ++ show reg ++ " " ++ show addr
    Mod      reg addr  -> "Mod "      ++ show reg ++ " " ++ show addr
    Abs      reg       -> "Abs "      ++ show reg
    Jump     offset    -> "Jump "     ++ show offset
    JumpCt   offset    -> "JumpCt "   ++ show offset
    JumpCf   offset    -> "JumpCf "   ++ show offset
    LoadMI   reg addr  -> "LoadMI "   ++ show reg ++ " " ++ show addr

    CmpEq    reg addr  -> "CmpEq " ++ show reg ++ " " ++ show addr
    CmpGt    reg addr  -> "CmpGt " ++ show reg ++ " " ++ show addr
    CmpLt    reg addr  -> "CmpLt " ++ show reg ++ " " ++ show addr

data Instruction a = forall c. Instruction (InstructionImpl c a)

instance Eq a => Eq (Instruction a) where
  (Instruction i) == (Instruction j) = i `instrEq` j

instrEq :: Eq a => InstructionImpl c1 a -> InstructionImpl c2 a -> Bool
instrEq i j = case (i, j) of
  (Halt, Halt) -> True
  (Load   reg1 addr1, Load   reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (Add    reg1 addr1, Add    reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (Sub    reg1 addr1, Sub    reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (Mul    reg1 addr1, Mul    reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (Div    reg1 addr1, Div    reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (Mod    reg1 addr1, Mod    reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (Store  reg1 addr1, Store  reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (Set    reg1 imm1 , Set    reg2 imm2 ) -> reg1 == reg2 && imm1  == imm2
  (Abs    reg1      , Abs    reg2      ) -> reg1 == reg2
  (Jump   offset1   , Jump   offset2   ) -> offset1 == offset2
  (JumpCt offset1   , JumpCt offset2   ) -> offset1 == offset2
  (JumpCf offset1   , JumpCf offset2   ) -> offset1 == offset2
  (LoadMI reg1 addr1, LoadMI reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (CmpEq  reg1 addr1, CmpEq  reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (CmpGt  reg1 addr1, CmpGt  reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (CmpLt  reg1 addr1, CmpLt  reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (_,_) -> False

mkI :: forall c a. InstructionImpl c a -> Instruction a
mkI = Instruction

instance Show (Instruction a) where
  show (Instruction i) = show i

-- -- | Programs are stored in program memory.
-- type InstructionAddress = Value

-- -- | Binary representation of an instruction
-- type InstructionCode = Word16

-- -- -- | Syntax of ISA instructions
-- -- data Instruction a = Ret
-- --                  -- ^ end execution
-- --                  --   @pc <- 0; halt@
-- --                  | Bnez Register (Imm a)
-- --                  -- ^ branch if the register contains nonzero
-- --                  --   @pc <- if (rs != 0) then iff else pc + 1
-- --                  | Sgtz Register{- @rd@ -} Register{- @rs@ -}
-- --                  -- ^ set if positive
-- --                  --   @pc <- pc + 1; rd <- if (rs > 0) then 1 else o
-- --                  | Sltz Register{- @rd@ -} Register{- @rs@ -}
-- --                  -- ^ set if positive
-- --                  --   @pc <- pc + 1; rd <- if (rs < 0) then 1 else o
-- --                  | Li Register (Imm a)
-- --                  -- ^ load immediate
-- --                  --   pc <- pc + 1; rd <- imm
-- --                  deriving (Show, Eq)
