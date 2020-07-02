{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Semantics
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Semantics of ISA instructions
-----------------------------------------------------------------------------
module ISA.Semantics
    ( instructionSemanticsS
    , instructionSemanticsM
    ) where

-- import           Control.Selective
import           Data.Bool             (bool)
import           Prelude               hiding (Monad, Read, read, readIO)
import qualified Prelude               (Monad)

import           FS
import           ISA.Selective
import           ISA.Types
import           ISA.Types.Instruction

type Monad f = (Selective f, Prelude.Monad f)

-----------------------------------------------------------------------------
-- -- | A valiant of 'Control.Selective.whenS' which, instead of returning @()@,
-- --   gives back the value of type @a@ on @True@ or @mempty@ on @False@.
-- --   This is essential to avoid redoing work just to make some semantics type-check.
-- whenS' :: (Selective f, Monoid a) => f Bool -> f a -> f a
-- whenS' x y = selector <*? effect
--   where
--     selector = bool (Right mempty) (Left ()) <$> x -- NB: maps True to Left ()
--     effect   = const                         <$> y
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
--------------- Semantics of instructions -----------------------------------
-----------------------------------------------------------------------------
-- | Halt the execution.
halt :: FS Key Applicative Value a
halt _ write = write (F Halted) (pure true)

load :: Register -> Address -> FS Key Functor Value a
load reg addr read write = write (Reg reg) (read (Addr addr))

loadMI :: Addressable a => Register -> Address -> FS Key Monad Value a
loadMI reg pointer read write =
  read (Addr pointer) >>= \x ->
    case toMemoryAddress x of
      Nothing   -> error $ "ISA.Semantics.loadMI: invalid address " <> show x
                 -- instead we actually need to rise a processor exception
      Just addr -> write (Reg reg) (read (Addr addr))

-- | Write an immediate argument to a register
set :: Register -> Imm a -> FS Key Applicative Value a
set reg (Imm imm) _ write =
  write (Reg reg) (pure imm)

store :: Register -> Address -> FS Key Functor Value a
store reg addr read write =
  write (Addr addr) (read (Reg reg))

add :: Register -> Address -> FS Key Selective Value a
add reg addr read write =
  let arg1 = read (Reg reg)
      arg2 = read (Addr addr)
      result = (+) <$> arg1 <*> arg2
      -- when @result@ is zero we set @Zero@ flag to @true@
  in write (Reg reg) result

addI :: Register -> Imm a -> FS Key Selective Value a
addI reg (Imm imm) read write =
  let arg1 = read (Reg reg)
      arg2 = pure imm
      result = (+) <$> arg1 <*> arg2
  in write (Reg reg) result

sub :: Register -> Address -> FS Key Selective Value a
sub reg addr read write =
  let arg1 = read (Reg reg)
      arg2 = read (Addr addr)
      result = (-) <$> arg1 <*> arg2
      -- when @result@ is zero we set @Zero@ flag to @true@
  in write (Reg reg) result

subI :: Register -> Imm a -> FS Key Selective Value a
subI reg (Imm imm) read write =
  let arg1 = read (Reg reg)
      arg2 = pure imm
      result = (-) <$> arg1 <*> arg2
  in write (Reg reg) result

-- | Compare the values in the register and memory cell
cmpEq :: Register -> Address -> FS Key Selective Value a
cmpEq reg addr = \read write ->
  select ((===) <$> read (Reg reg)
                <*> read (Addr addr))
  (\x -> if x then (write (F Condition) (pure true))
              else (write (F Condition) (pure (ISA.Types.not true))))
  -- (write (F Condition) id)
  (pure id)
    -- whenS (toBool <$> ((===) <$> read (Reg reg)
    --                          <*> read (Addr addr))
    --       )
    --       (write (F Condition) (pure true))

cmpGt :: Register -> Address -> FS Key Selective Value a
cmpGt reg addr = \read write ->
  whenS (toBool <$> (gt <$> read (Reg reg) <*> read (Addr addr)))
        (write (F Condition) (pure true))

cmpLt :: Register -> Address -> FS Key Selective Value a
cmpLt reg addr = \read write ->
  select (lt <$> read (Reg reg)
             <*> read (Addr addr))
  (\x -> if x then (write (F Condition) (pure true))
              else (write (F Condition) (pure (ISA.Types.not true))))
  -- (write (F Condition) id)
  (pure id)
  -- whenS (toBool <$> (lt <$> read (Reg reg) <*> read (Addr addr)))
  --       (write (F Condition) (pure true))

-- | Perform jump if flag @Condition@ is set
jumpCt :: Imm a -> FS Key Selective Value a
jumpCt (Imm offset) read write =
  whenS (toBool <$> read (F Condition))
        (write IC ((+) <$> pure offset
                       <*> read IC))

-- | Perform unconditional jump
jump :: Imm a -> FS Key Applicative Value a
jump (Imm offset) read write =
  write IC ((+) <$> pure offset <*> read IC)

-----------------------------------------------------------------------------

-- | Increment the instruction counter.
incrementIC :: Value a => FS Key Functor Value a
incrementIC read write =
  write IC ((+ 1) <$> read IC)

-- -- | Aha! fetching an instruction is Monadic!
-- fetchInstruction :: Value a => FS Key Prelude.Monad Value a
-- fetchInstruction read write =
--       read IC >>= \ic -> write IR (read (Prog ic))

instructionSemanticsS :: Value a => Instruction a -> FS Key Selective Value a
instructionSemanticsS (Instruction i) r w = case i of
    Halt           -> halt r w
    Load reg addr  -> load reg addr r w
    LoadMI reg addr ->
      error $ "ISA.Semantics.instructionSemanticsS : "
           ++ "LoadMI does not have Selective semantics "
    Set reg imm    -> set reg imm r w
    Store reg addr -> store reg addr r w
    Add reg addr   -> add reg addr r w
    AddI reg imm   -> addI reg imm r w
    Sub reg addr   -> sub reg addr r w
    SubI reg addr   -> subI reg addr r w
    Mul reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    Div reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    Mod reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    Abs reg        -> error "ISA.Semantics.instructionSemantics : not implemented"
    Jump simm8     -> jump simm8 r w

    CmpEq reg addr -> cmpEq reg addr r w
    CmpGt reg addr -> cmpGt reg addr r w
    CmpLt reg addr -> cmpLt reg addr r w

    JumpCt simm8   -> jumpCt simm8 r w
    JumpCf simm8   -> error "ISA.Semantics.instructionSemantics : not implemented"


instructionSemanticsM ::
  (Addressable a, Value a) => Instruction a -> FS Key Monad Value a
instructionSemanticsM (Instruction i) r w = case i of
  LoadMI reg addr -> loadMI reg addr r w
  _               -> instructionSemanticsS (Instruction i) r w
