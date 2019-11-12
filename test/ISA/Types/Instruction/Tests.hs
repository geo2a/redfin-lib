{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ISA.Types.Instruction.Tests where

import           Data.Int                     (Int32)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Instruction.Encode
-----------------------------------------------------------------------------
encodedecode :: TestTree
encodedecode =
  testGroup "Types.Instruction.Decode/Encode"
    [ testProperty "decoder is a revers of encoder" $
        \(icode :: InstructionCode (Data Int32)) ->
           icode > 0 ==>
             case decode icode of
               Just i  -> encode i == icode
               Nothing -> True
    ]
-----------------------------------------------------------------------------
