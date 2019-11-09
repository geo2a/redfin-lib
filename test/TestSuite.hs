{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.HUnit ()

import           ISA.SMT.Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testGroup "SExpr to SMT translation and SMT solving"
    [ gatherFreeTests
    , satTests
    ]
  ]

-----------------------------------------------------------------------------
