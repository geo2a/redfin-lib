{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{- |
 Module     : ISA.Example.Sum
 Copyright  : (c) Georgy Lukyanov 2019-2020
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Example program that finds the sum of numbers in an array
-}
module ISA.Example.Sum (
    demo,
    sumArrayLowLevel,
    initCtx,
) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Prelude hiding (not)

import ISA.Assembly
import ISA.Backend.Graph
import ISA.Backend.Graph.BasicBlock
import ISA.Backend.Symbolic.Zipper.Run
import ISA.Types
import ISA.Types.Boolean
import ISA.Types.Context
import ISA.Types.Key
import ISA.Types.Symbolic
import ISA.Types.Symbolic.ACTL
import ISA.Types.Symbolic.ACTL.Model
import ISA.Types.Symbolic.Address

sumArrayLowLevel :: Script
sumArrayLowLevel = do
    let pointer = 0; sum = 253; array_start = 255 -- ; pointer_store
    let r0 = R0; r1 = R1; r2 = R2
    -- ld_i r0 0
    -- st r0 sum
    ld r1 pointer
    -- add_i r1 1
    -- st r1 pointer

    -- compare the pointer variable to the array_start
    "loop" @@ cmplt r1 array_start
    -- if pointer == array_start then terminate
    goto_ct "end"
    -- jmpi_ct 7

    ldmi r2 pointer
    add r2 sum
    st r2 sum
    -- ld r1 pointer
    sub_i r1 1
    st r1 pointer

    goto "loop"
    "end" @@ ld r0 sum
    halt

array :: CAddress -> Int -> (Sym -> Sym) -> ([(Key, Sym)], [(Text.Text, Sym)])
array start size varConstraint =
    let addrs = [start .. start + fromIntegral size]
        vars = map SAny . map ("x" <>) . map (Text.pack . show) $ addrs
        constraints = map varConstraint vars
     in ( zip (map (Addr . literal) addrs) vars
        , zip (map (Text.pack . show) constraints) constraints
        )

initCtx :: Context Sym
initCtx =
    let (vars, constrs) = array 1 n (\x -> (SGt x 0) &&& (SLt x 10000))
        n = 10
     in MkContext
            { _pathCondition = true
            , _store = Map.empty
            , _constraints =
                constrs
                    ++ [ ("n", (SGt (SAny "n") (0 -1)) &&& (SLt (SAny "n") (SConst (CInt32 $ fromIntegral n))))
                    -- , ("n", (SLt (SAny "n") 4))
                    -- , ("n", (SLt (SAny "n") 4))
                       ]
            , _bindings =
                Map.fromList $
                    [ (IC, 0)
                    , (Reg R0, 0)
                    , (Reg R1, 0)
                    , (Reg R2, 0)
                    , (Addr 0, SAny "n")
                    , (Addr 253, 0)
                    , (Addr 255, 1)
                    ]
                        ++ vars
                        ++ [ (F Halted, false)
                           , (F Condition, false)
                           , (F Overflow, false)
                           ]
                        ++ mkProgram sumArrayLowLevel
            , _solution = Nothing
            }

----- Properties
all_finally_halted = either undefined id (parseTheorem "" "F ([Halted])")

all_globally_no_overflow = either undefined id (parseTheorem "" "G (![Overflow])")

----- Demo
demo :: IO ()
demo = do
    trace <- runModel 100 initCtx
    r1 <- prove trace all_finally_halted
    print r1
    -- print (solution r1)
    r2 <- prove trace all_globally_no_overflow
    print r2
    pure ()
