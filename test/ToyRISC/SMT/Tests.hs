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
          gatherFree (SAny "x") @?= Set.fromList [SAny "x"]
      , testCase "Any as arguments of operations" $
          gatherFree (SGt (SAny "x") (SAny "y")) @?=
          Set.fromList [SAny "x", SAny "y"]
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

testExpr1 :: Sym Bool
testExpr1 = let x = SAny "x"
                y = SAny "y"
                xGt10 = SGt x (SConst 10)
                yLt0  = SLt y (SConst 0)
                xEqY = SEq x y
            in SAnd xEqY (SAnd xGt10 yLt0)

testExpr2 :: Sym Bool
testExpr2 = let x = SAny "x"
                y = SAny "y"
                xGt10 = SGt x (SConst 10)
                yGt0  = SGt y (SConst 0)
                sumGt10 s = SGt s (SConst 10)
            in conjoin [sumGt10 (SAdd x y), xGt10, yGt0]

plusComm :: Sym Bool
plusComm = let x = SAny "x"
               y = SAny "y"
           in SNot $ SEq (x + y) (y + x)
-----------------------------------------------------------------------------
