{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

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
    -- ( Instruction(..)
    -- )
  where

import           Control.Selective
import           Data.Int          (Int16, Int64, Int8)
import           Data.Kind         (Constraint)
import           Data.Word         (Word16, Word64, Word8)
import           Prelude           hiding (Read, readIO)
import           Prelude           hiding (Monad, Read, abs, div, mod)
import qualified Prelude           (Monad, Read, abs, div, mod)

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
  JumpZero :: Value a => Imm a                   -> InstructionImpl Selective a
  LoadMI   :: Register -> Address -> InstructionImpl Prelude.Monad a

  CmpEq    :: Register  -> Address -> InstructionImpl Selective a
  CmpGt    :: Register  -> Address -> InstructionImpl Selective a
  CmpLt    :: Register  -> Address -> InstructionImpl Selective a

  JumpCt   :: Value a => Imm a                     -> InstructionImpl Selective a
  JumpCf   :: Value a => Imm a                     -> InstructionImpl Selective a

-- deriving instance Eq  (InstructionImpl c)
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
    JumpZero offset    -> "JumpZero " ++ show offset
    JumpCt   offset    -> "JumpCt "   ++ show offset
    JumpCf   offset    -> "JumpCf "   ++ show offset
    LoadMI   reg addr  -> "LoadMI "   ++ show reg ++ " " ++ show addr

    CmpEq    reg addr  -> "CmpEq " ++ show reg ++ " " ++ show addr
    CmpGt    reg addr  -> "CmpGt " ++ show reg ++ " " ++ show addr
    CmpLt    reg addr  -> "CmpLt " ++ show reg ++ " " ++ show addr

data Instruction a = forall c. Instruction (InstructionImpl c a)

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
