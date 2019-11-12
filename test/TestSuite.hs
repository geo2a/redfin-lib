{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.HUnit            ()

import           ISA.SMT.Tests
import           ISA.Types.Instruction.Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testGroup "SExpr to SMT translation and SMT solving"
    [ gatherFreeTests
    , satTests
    ]
  ]

properties :: TestTree
properties = testGroup "QuickCheck properties"
  [encodedecode]
-----------------------------------------------------------------------------
