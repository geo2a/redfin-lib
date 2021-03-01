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
    , Unconstrained
    )
  where

import           Control.Selective
import           Data.Int          ()
import           Data.Kind         (Constraint)
import           Data.Word         ()
import           Prelude           hiding (Read, abs, div, mod, readIO)

import           ISA.Types

--------------------------------------------------------------------------------
---------------- Instructions --------------------------------------------------
--------------------------------------------------------------------------------

class Unconstrained (a :: * -> *)
instance Unconstrained a

data InstructionImpl (control :: (* -> *) -> Constraint) (value :: * -> Constraint) a where
  Halt     :: InstructionImpl Applicative value a
  Load     :: Register -> CAddress -> InstructionImpl Functor value a
  Set      :: value a => Register -> Imm a         -> InstructionImpl Functor value a
  Store    :: Register -> CAddress -> InstructionImpl Functor value a
  Add      :: Register -> CAddress -> InstructionImpl Applicative value a
  AddI     :: value a => Register -> Imm a -> InstructionImpl Applicative value a
  Sub      :: Register -> CAddress -> InstructionImpl Applicative value a
  SubI     :: value a => Register -> Imm a -> InstructionImpl Applicative value a
  Mul      :: Register -> CAddress -> InstructionImpl Applicative value a
  Div      :: Register -> CAddress -> InstructionImpl Applicative value a
  Mod      :: Register -> CAddress -> InstructionImpl Applicative value a
  Abs      :: Register                  -> InstructionImpl Applicative value a
  Jump     :: value a => Imm a                    -> InstructionImpl Applicative value a
  LoadMI   :: Register -> CAddress -> InstructionImpl Prelude.Monad value a

  CmpEq    :: Register  -> CAddress -> InstructionImpl Selective value a
  CmpGt    :: Register  -> CAddress -> InstructionImpl Selective value a
  CmpLt    :: Register  -> CAddress -> InstructionImpl Selective value a

  JumpCt   :: value a => Imm a -> InstructionImpl Selective value a
  JumpCf   :: value a => Imm a -> InstructionImpl Selective value a

deriving instance Eq a => Eq (InstructionImpl c v a)

instance Show a => Show (InstructionImpl control value a) where
  show = \case
    Halt               -> "Halt"
    Load     reg addr  -> "Load "     ++ show reg ++ " " ++ show addr
    Set      reg value -> "Set "      ++ show reg ++ " " ++ show value
    Store    reg addr  -> "Store "    ++ show reg ++ " " ++ show addr
    Add      reg addr  -> "Add "      ++ show reg ++ " " ++ show addr
    AddI     reg imm   -> "AddI "     ++ show reg ++ " " ++ show imm
    Sub      reg addr  -> "Sub "      ++ show reg ++ " " ++ show addr
    SubI     reg imm   -> "SubI "     ++ show reg ++ " " ++ show imm
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

data Instruction a = forall c v. v a => Instruction (InstructionImpl c v a)

instance Eq a => Eq (Instruction a) where
  (Instruction i) == (Instruction j) = i `instrEq` j

mapInstructionImpl :: forall c v a b. (v a, v b)
                   => (a -> b)
                   -> InstructionImpl c v a
                   -> InstructionImpl c v b
mapInstructionImpl f = \case
   Halt           -> Halt
   Load r a       -> Load r a
   Set r (Imm i)  -> Set r (Imm (f i))
   Store r a      -> Store r a
   Add r a        -> Add r a
   AddI r (Imm i) -> AddI r (Imm (f i))
   Sub r a        -> Sub r a
   SubI r (Imm i) -> SubI r (Imm (f i))
   Mul r a        -> Mul r a
   Div r a        -> Div r a
   Mod r a        -> Mod r a
   Abs r          -> Abs r
   Jump (Imm i)   -> Jump (Imm (f i))
   LoadMI r a     -> LoadMI r a
   CmpEq r a      -> CmpEq r a
   CmpGt r a      -> CmpGt r a
   CmpLt r a      -> CmpLt r a
   JumpCt (Imm i) -> JumpCt (Imm (f i))
   JumpCf (Imm i) -> JumpCf (Imm (f i))

instrEq :: Eq a => InstructionImpl c1 v1 a -> InstructionImpl c2 v2 a -> Bool
instrEq i j = case (i, j) of
  (Halt, Halt)                           -> True
  (Load   reg1 addr1, Load   reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (Add    reg1 addr1, Add    reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (AddI   reg1 imm1 , AddI   reg2 imm2 ) -> reg1 == reg2 && imm1 == imm2
  (Sub    reg1 addr1, Sub    reg2 addr2) -> reg1 == reg2 && addr1 == addr2
  (SubI   reg1 imm1 , SubI   reg2 imm2 ) -> reg1 == reg2 && imm1 == imm2
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
  (_,_)                                  -> False

mkI :: forall c v a. v a => InstructionImpl c v a -> Instruction a
mkI = Instruction

instance Show a => Show (Instruction a) where
  show (Instruction i) = show i
