{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ISA.Types.Instruction.Tests where

import Data.Int (Int32)
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.QuickCheck

import ISA.Types
import ISA.Types.Instruction
import ISA.Types.Instruction.Decode
import ISA.Types.Instruction.Encode

-----------------------------------------------------------------------------
encodedecode :: TestTree
encodedecode =
    testGroup
        "Types.Instruction.Decode/Encode"
        [ testProperty "decoder is a revers of encoder" $
            \(icode :: InstructionCode) ->
                icode > 0
                    ==> case decode icode of
                        Just i ->
                            case decode (encode i) of
                                Just j -> i == j
                                Nothing -> True
                        Nothing -> True
        ]

t :: IO ()
t = do
    let ic = InstructionCode 8
    let d = fromJust $ decode ic
    let e = fromJust $ decode $ encode d
    print e

-----------------------------------------------------------------------------
