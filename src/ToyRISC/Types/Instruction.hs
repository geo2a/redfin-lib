{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Instruction
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Syntax of ToyRISC instructions
-----------------------------------------------------------------------------
module ToyRISC.Types.Instruction
    ( Instruction(..)
    ) where

import           Prelude       hiding (Read, readIO)

import           ToyRISC.Types

-----------------------------------------------------------------------------

-- | Syntax of ToyRISC instructions
data Instruction a = Ret
                 -- ^ end execution
                 --   @pc <- 0; halt@
                 | Bnez Register (Imm a)
                 -- ^ branch if the register contains nonzero
                 --   @pc <- if (rs != 0) then iff else pc + 1
                 | Sgtz Register{- @rd@ -} Register{- @rs@ -}
                 -- ^ set if positive
                 --   @pc <- pc + 1; rd <- if (rs > 0) then 1 else o
                 | Sltz Register{- @rd@ -} Register{- @rs@ -}
                 -- ^ set if positive
                 --   @pc <- pc + 1; rd <- if (rs < 0) then 1 else o
                 | Li Register (Imm a)
                 -- ^ load immediate
                 --   pc <- pc + 1; rd <- imm
                 deriving (Show, Eq)
