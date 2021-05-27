{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{- |
 Module     : ISA.Example.Add
 Copyright  : (c) Georgy Lukyanov 2019-2020
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental
-}
module ISA.Example.Add (demo, addLowLevel, initCtx) where

import qualified Data.Map as Map

import ISA.Assembly
import ISA.Backend.Symbolic.Zipper
import ISA.Backend.Symbolic.Zipper.Run
import ISA.Types
import ISA.Types.Boolean
import ISA.Types.Context
import ISA.Types.Key
import ISA.Types.Symbolic
import ISA.Types.Tree

addLowLevel :: Script
addLowLevel = do
    let x = 0; y = 1 -- ; pointer_store
    let r0 = R0
    ld r0 x
    add r0 y
    halt

initCtx :: Context Sym
initCtx =
    MkContext
        { _pathCondition = true
        , _constraints = []
        , _bindings =
            Map.fromList $
                [ (IC, 0)
                , (Reg R0, 0)
                , (Addr 0, SAny "x")
                , (Addr 1, SAny "y")
                ]
                    ++ mkProgram addLowLevel
        , _store = Map.empty
        , _solution = Nothing
        }
demo :: IO ()
demo = do
    tr <- runModel 10 initCtx
    mapM_ putStrLn (draw (_layout tr))
    mapM_ (putStrLn . show) (_states tr)
    -- mapM_ putStr$ fmap (\(Node n ctx) -> showContext ctx) (unTrace tr)
    -- let noOverflow = (AllG . Not . Atom $
    --                  (\ctx -> maybe undefined id
    --                         . getBinding (F Overflow) $ ctx))
    --     correct = (AllF . Atom $
    --               (\ctx -> SEq (SAdd (SAny "x") (SAny "y")) . maybe undefined id
    --                      . getBinding (Reg R0) $ ctx))
    --     halted = (AllF . Atom $
    --               (\ctx -> maybe undefined id
    --                      . getBinding (F Halted) $ ctx)
    -- let phi = And noOverflow (And halted correct)
    -- putStrLn $ "Property: " <> show (formulate (Not phi) tr)
    -- putStrLn $ "Property': " <> show (formulate psi tr)
    -- ans <- prove phi tr $
    --        ConstrainedBy $ [ (SAny "x" `SGt` 0) &&& (SAny "x" `SLt` 1000)
    --                        -- , (SAny "y" `SGt` 0) &&& (SAny "y" `SLt` 1000)
    --                        ]
    -- print ans
    pure ()
