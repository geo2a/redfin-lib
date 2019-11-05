{-# LANGUAGE OverloadedStrings #-}

module ToyRISC.SMT.Tests where

import qualified Data.SBV.Dynamic as SBV ()
import qualified Data.Set         as Set
import           Test.Tasty
import           Test.Tasty.HUnit

import           ToyRISC.SMT
import           ToyRISC.Symbolic
-----------------------------------------------------------------------------
gatherFreeTests :: TestTree
gatherFreeTests =
  testGroup "SMT.gatherFree"
      [ testCase "singleton Any" $
          gatherFree (Any "x") @?= Set.fromList [Any "x"]
      , testCase "Any as arguments of operations" $
          gatherFree (Symbolic Eq [Symbolic And [Any "x", constBool True], Any "y"]) @?=
          Set.fromList [Any "x", Any "y"]
      ]
-----------------------------------------------------------------------------
satTests :: TestTree
satTests = testGroup "SMT.sat"
      [ testCase "[x == y | x > 10 && y < 0] is unsatisfiable" $ do
          result <- sat testExpr1
          assertBool (show result) (isUnsat result)
      , testCase "[x + y > 10 | x > 10 && y > 0] is satisfiable" $ do
          result <- sat testExpr2
          assertBool (show result) (isSat result)
      ]

testExpr1 :: SExpr
testExpr1 = let x = Any "x"
                y = Any "y"
                xGt10 = sOp Gt [x, sConst 10]
                yLt0  = sOp Lt [y, sConst 0]
                xEqY = sOp Eq [x, y]
            in sOp And [xEqY, sOp And [xGt10, yLt0]]

testExpr2 :: SExpr
testExpr2 = let x = Any "x"
                y = Any "y"
                xGt10 = sOp Gt [x, sConst 10]
                yGt0  = sOp Gt [y, sConst 0]
                sumGt10 s = sOp Gt [s, sConst 10]
            in conjoin [sumGt10 (sOp Plus [x, y]), xGt10, yGt0]
-----------------------------------------------------------------------------
