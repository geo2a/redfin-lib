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
    ( halt
    , add
    , jumpCt
    , whenS'
    , instructionSemantics
    ) where

import           Control.Selective
import           Data.Bool             (bool)
import           Prelude               hiding (Read, read, readIO)

import           FS
import           ISA.Types
import           ISA.Types.Instruction

-----------------------------------------------------------------------------
-- | A valiant of 'Control.Selective.whenS' which, instead of returning @()@,
--   gives back the value of type @a@ on @True@ or @mempty@ on @False@.
--   This is essential to avoid redoing work just to make some semantics type-check.
whenS' :: (Selective f, Monoid a) => f Bool -> f a -> f a
whenS' x y = selector <*? effect
  where
    selector = bool (Right mempty) (Left ()) <$> x -- NB: maps True to Left ()
    effect   = const                         <$> y
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
--------------- Semantics of instructions -----------------------------------
-----------------------------------------------------------------------------
-- | Halt the execution.
halt :: FS Key Applicative Value a
halt _ write = write (F Halted) (pure true)

-- | Write an immediate argument to a register
set :: Register -> Imm a -> FS Key Applicative Value a
set reg (Imm imm) _ write =
    write (Reg reg) (pure imm)

add :: Register -> Address -> FS Key Selective Value a
add reg addr read write =
  let arg1 = read (Reg reg)
      arg2 = read (Addr addr)
      result = (+) <$> arg1 <*> arg2
      -- when @result@ is zero we set @Zero@ flag to @true@
  in whenS' (toBool <$> ((===) <$> write (Reg reg) result
                               <*> pure (fromInteger 0))
            )
            (write (F Condition) (pure true))

-- | Compare the values in the register and memory cell
cmpEq :: Register -> Address -> FS Key Selective Value a
cmpEq reg addr = \read write ->
    whenS' (toBool <$> ((===) <$> read (Reg reg)
                              <*> read (Addr addr))
           )
           (write (F Condition) (pure true))

-- cmpGt :: Register -> Address -> FS Key Applicative Value a
-- cmpGt reg addr = \read write ->
--     write (F Condition) (S <$> read (Reg reg) <*> read (Addr addr))

-- cmpLt :: Register -> Address -> FS Key Applicative Value a
-- cmpLt reg addr = \read write ->
--     write (F Condition) (SLt <$> read (Reg reg) <*> read (Addr addr))

-- | Perform jump if flag @Condition@ is set
jumpCt :: Imm a -> FS Key Selective Value a
jumpCt (Imm offset) read write =
    whenS' (toBool <$> read (F Condition))
           (write IC ((+) <$> pure offset
                          <*> read IC))
-----------------------------------------------------------------------------

-- | Increment the instruction counter.
incrementIC :: Value a => FS Key Functor Value a
incrementIC read write =
  write IC ((+ 1) <$> read IC)

-- -- | Aha! fetching an instruction is Monadic!
-- fetchInstruction :: Value a => FS Key Prelude.Monad Value a
-- fetchInstruction read write =
--       read IC >>= \ic -> write IR (read (Prog ic))

instructionSemantics :: Value a => Instruction a -> FS Key Selective Value a
instructionSemantics (Instruction i) r w = case i of
    Halt           -> halt r w
    Load reg addr  -> error "ISA.Semantics.instructionSemantics : not implemented"
    LoadMI reg addr -> error "ISA.Semantics.instructionSemantics : not implemented"
    Set reg imm    -> set reg imm r w
    Store reg addr -> error "ISA.Semantics.instructionSemantics : not implemented"
    Add reg addr   -> add reg addr r w
    AddI reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    Sub reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    SubI reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    Mul reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    Div reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    Mod reg addr   -> error "ISA.Semantics.instructionSemantics : not implemented"
    Abs reg        -> error "ISA.Semantics.instructionSemantics : not implemented"
    Jump simm8     -> error "ISA.Semantics.instructionSemantics : not implemented"

    CmpEq reg addr -> cmpEq reg addr r w
    CmpGt reg addr -> error "ISA.Semantics.instructionSemantics : not implemented"
    CmpLt reg addr -> error "ISA.Semantics.instructionSemantics : not implemented"

    JumpCt simm8   -> jumpCt simm8 r w
    JumpCf simm8   -> error "ISA.Semantics.instructionSemantics : not implemented"
