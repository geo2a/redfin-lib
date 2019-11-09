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

instructionSemantics :: Value a => Instruction a -> FS Key Selective Value a
instructionSemantics (Instruction i) r w = case i of
    Halt           -> halt r w
    -- Load reg addr  -> load reg addr r w
    -- -- -- LoadMI reg addr -> loadMI reg addr r w
    Set reg imm    -> set reg imm r w
    -- Store reg addr -> store reg addr r w
    Add reg addr   -> add reg addr r w
    -- Sub reg addr   -> sub reg addr r w
    -- Mul reg addr   -> mul reg addr r w
    -- Div reg addr   -> div reg addr r w
    -- Mod reg addr   -> mod reg addr r w
    -- Abs reg        -> abs reg r w
    -- Jump simm8     -> jump simm8 r w
    -- JumpZero simm8 -> jumpZero simm8 r w

    CmpEq reg addr -> cmpEq reg addr r w
    -- CmpGt reg addr -> cmpGt reg addr r w
    -- CmpLt reg addr -> cmpLt reg addr r w

    JumpCt simm8   -> jumpCt simm8 r w
    -- JumpCf simm8   -> jumpCf simm8 r w
