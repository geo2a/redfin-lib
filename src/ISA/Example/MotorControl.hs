{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{- |
 Module     : ISA.Example.MotorControl
 Copyright  : (c) Georgy Lukyanov 2019
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental
-}
module ISA.Example.MotorControl where

import Prelude hiding (div, mod)

import Data.Int (Int32)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Prelude hiding (div, not)

import ISA.Assembly
import ISA.Backend.Graph
import ISA.Backend.Graph.BasicBlock
import ISA.Backend.Symbolic.Zipper
import ISA.Backend.Symbolic.Zipper.Run
import ISA.Backend.Symbolic.Zipper.Save
import ISA.Types
import ISA.Types.Boolean
import ISA.Types.Context
import ISA.Types.Key
import ISA.Types.Symbolic
import ISA.Types.Symbolic.ACTL
import ISA.Types.Symbolic.ACTL.Model
import ISA.Types.Symbolic.Address

import ISA.Backend.Graph

-- | The loop body of a stepper motor control program
mc_loop :: Script
mc_loop = do
    let a_max = 0
        v_max = 1
        dist = 2
        s = 3
        v = 4
        s_decel = 5
        decel_steps = 6
        temp = 7
        v_next = 8
        r0 = R0
        r1 = R1
        r2 = R2
        r3 = R3
    ld r0 v
    div r0 a_max
    st r0 decel_steps
    ld_i r1 1
    st r1 temp
    add r0 temp
    st r0 temp
    ld r0 a_max
    mul r0 decel_steps
    mul r0 temp
    -- sra_i r0 1
    div r0 a_max

    ld r1 decel_steps
    mul r1 a_max
    cmpeq r1 v
    goto_ct "store_s_decel"
    add r0 v
    "store_s_decel" @@ st r0 s_decel

    -- compute v_next
    ld r0 v
    add r0 a_max -- v_next = v + a_max
    cmplt r0 dist -- v_next < dist ?
    goto_ct "keep_v_next1"
    ld r0 dist -- overwrite v_next with dist
    "keep_v_next1" @@ cmplt r0 v_max -- v_next < v_max ?
    goto_ct "keep_v_next2"
    ld r0 v_max -- overwrite v_next with v_max
    "keep_v_next2" @@ st r0 v_next -- store v_next value

    -- set speed according to final distance
    add r0 s_decel
    add r0 s
    cmpgt r0 dist -- s + s_decel + v_next > dist ?
    goto_ct "keep_speed"
    ld r1 v_next -- accelerate
    goto "set_v"
    "keep_speed" @@ ld r0 s
    add r0 s_decel
    add r0 v
    cmpgt r0 dist -- s + s_decel + v > dist ?
    goto_ct "decelerate"
    ld r1 v
    goto "set_v"
    "decelerate" @@ ld r0 v
    ld r1 decel_steps
    mul r1 a_max -- n * a_max
    st r1 temp
    cmpgt r0 temp -- v > n * a_max ?
    goto_ct "set_v"
    ld r1 v
    sub r1 a_max
    "set_v" @@ st r1 v

    -- speed check
    ld_i r0 0
    st r0 temp
    cmpeq r1 temp -- v == 0?
    goto_cf "inc_s" -- speed is non-zero: continue

    -- in case speed is 0, check distance covered:
    ld r0 s
    cmpeq r0 dist
    goto_cf "reaccelerate"
    halt -- we have reached our destination
    "reaccelerate" @@ ld r0 dist
    sub r0 s -- dist - s
    cmplt r0 a_max
    goto_ct "set_v2"
    ld r0 a_max
    "set_v2" @@ st r0 v

    "inc_s" @@ ld r0 s
    add r0 v
    st r0 s

    halt

mc :: Script
mc = do
    "start" @@ mc_loop
    goto "start"

initCtx :: Context Sym
initCtx =
    MkContext
        { _pathCondition = true
        , _constraints =
            [ ("0 < a_max < 10", (SGt a_max 0) &&& (SLt a_max 100))
            , ("0 < v_max < 100", (SGt v_max 0) &&& (SLt v_max 100))
            , ("0 < dist < 1000", (SGt dist 0) &&& (SLt dist 1000))
            , ("0 < s < 1000", (SGt s 0) &&& (SLt s 1000))
            , ("0 < v < v_max", (SGt v 0) &&& (SLt v v_max))
            ]
        , _bindings =
            Map.fromList $
                [ (IC, 0)
                , (IR, 0)
                , (F Condition, false)
                , (F Halted, false)
                , (F Overflow, false)
                , (Addr 0, SAny "a_max")
                , (Addr 1, SAny "v_max")
                , (Addr 2, SAny "dist")
                , (Addr 3, SAny "s")
                , (Addr 4, SAny "v")
                ]
                    ++ mkProgram mc_loop
        , _store = Map.empty
        , _solution = Nothing
        }
  where
    a_max = SAny "a_max"
    v_max = SAny "v_max"
    dist = SAny "dist"
    s = SAny "s"
    v = SAny "v"

----- Properties
all_finally_halted = either undefined id (parseTheorem "" "F ([Halted])")

all_globally_no_overflow = either undefined id (parseTheorem "" "G (![Overflow])")

proveOnTraceFromFile :: FilePath -> ACTL -> IO ()
proveOnTraceFromFile tracePath prop = do
    trace <- either (error . Text.unpack) id <$> loadTrace tracePath
    prove trace prop
    pure ()

----- Demos
demo :: IO ()
demo = do
    trace <- runModel 1000 initCtx
    saveTrace "traces/mc_loop_halted.json" trace
    -- trace <- either undefined id <$> loadTrace "traces/mc_loop_halted.json"
    r1 <- prove trace all_finally_halted
    r2 <- prove trace all_globally_no_overflow
    print (r1, r2)
    pure ()
