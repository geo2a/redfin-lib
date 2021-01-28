{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Example.MotorControl
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Module desription
--
-----------------------------------------------------------------------------

module ISA.Example.MotorControl
    (mc_loop, initCtx) where

import           Control.Monad                      (filterM)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Selective
import           Data.Foldable                      (sequenceA_)
import           Data.Int
import           Data.Maybe                         (fromJust)
import           Prelude                            hiding (div, mod)
import           System.CPUTime
import           System.IO.Unsafe                   (unsafePerformIO)
import           Text.Pretty.Simple                 (pPrint)
import           Text.Printf
-- import qualified Data.Tree as Tree
import qualified Data.Map.Strict                    as Map
import qualified Data.SBV                           as SBV

import           ISA.Assembly
import           ISA.Backend.Dependencies
import           ISA.Backend.Symbolic.List.QueryRun
import           ISA.Backend.Symbolic.QueryList
import           ISA.Example.Common
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Instruction.Encode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace


-- -- | The loop body of a stepper motor control program.
-- mc_loop :: Script
-- mc_loop = do
--     let { a_max = 0; v_max = 1; dist = 2; s = 3; v = 4; s_decel = 5;
--           decel_steps = 6; temp = 7; v_next = 8; }
--     ld r0 v
--     div r0 a_max
--     st r0 decel_steps
--     ld_si r1 1
--     st r1 temp
--     add r0 temp
--     st r0 temp
--     ld r0 a_max
--     mul r0 decel_steps
--     mul r0 temp
--     -- sra_i r0 1
--     div r0 a_max

--     ld r1 decel_steps
--     mul r1 a_max
--     cmpeq r1 v
--     goto_ct "store_s_decel"
--     add r0 v
--     "store_s_decel" @@ st r0 s_decel

--     -- compute v_next
--     ld r0 v
--     add r0 a_max                        -- v_next = v + a_max
--     cmplt r0 dist                       -- v_next < dist ?
--     goto_ct "keep_v_next1"
--     ld r0 dist                          -- overwrite v_next with dist
--     "keep_v_next1" @@ cmplt r0 v_max    -- v_next < v_max ?
--     goto_ct "keep_v_next2"
--     ld r0 v_max                         -- overwrite v_next with v_max
--     "keep_v_next2" @@ st r0 v_next      -- store v_next value

--     -- set speed according to final distance
--     add r0 s_decel
--     add r0 s
--     cmpgt r0 dist                       -- s + s_decel + v_next > dist ?
--     goto_ct "keep_speed"
--     ld r1 v_next                        -- accelerate
--     goto "set_v"
--     "keep_speed" @@ ld r0 s
--     add r0 s_decel
--     add r0 v
--     cmpgt r0 dist                       -- s + s_decel + v > dist ?
--     goto_ct "decelerate"
--     ld r1 v
--     goto "set_v"
--     "decelerate" @@ ld r0 v
--     ld r1 decel_steps
--     mul r1 a_max                       -- n * a_max
--     st r1 temp
--     cmpgt r0 temp                      -- v > n * a_max ?
--     goto_ct "set_v"
--     ld r1 v
--     sub r1 a_max
--     "set_v" @@ st r1 v

--     -- speed check
--     ld_si r0 0
--     st r0 temp
--     cmpeq r1 temp                     -- v == 0?
--     goto_cf "inc_s"                   -- speed is non-zero: continue

--     -- in case speed is 0, check distance covered:
--     ld r0 s
--     cmpeq r0 dist
--     goto_cf "reaccelerate"
--     halt                              -- we have reached our destination
--     "reaccelerate" @@ ld r0 dist
--     sub r0 s                          -- dist - s
--     cmplt r0 a_max
--     goto_ct "set_v2"
--     ld r0 a_max
--     "set_v2" @@ st r0 v

--     "inc_s" @@ ld r0 s
--     add r0 v
--     st r0 s

--     halt

-- | The loop body of a stepper motor control program.
mc_loop :: Script
mc_loop = do
    let { a_max = 0; v_max = 1; dist = 2; s = 3; v = 4; s_decel = 5;
          decel_steps = 6; temp = 7; v_next = 8;
          r0 = R0; r1 = R1; r2 = R2; r3 = R3 }
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
    add r0 a_max                        -- v_next = v + a_max
    cmplt r0 dist                       -- v_next < dist ?
    goto_ct "keep_v_next1"
    ld r0 dist                          -- overwrite v_next with dist
    "keep_v_next1" @@ cmplt r0 v_max    -- v_next < v_max ?
    goto_ct "keep_v_next2"
    ld r0 v_max                         -- overwrite v_next with v_max
    "keep_v_next2" @@ st r0 v_next      -- store v_next value

    -- set speed according to final distance
    add r0 s_decel
    add r0 s
    cmpgt r0 dist                       -- s + s_decel + v_next > dist ?
    goto_ct "keep_speed"
    ld r1 v_next                        -- accelerate
    goto "set_v"
    "keep_speed" @@ ld r0 s
    add r0 s_decel
    add r0 v
    cmpgt r0 dist                       -- s + s_decel + v > dist ?
    goto_ct "decelerate"
    ld r1 v
    goto "set_v"
    "decelerate" @@ ld r0 v
    ld r1 decel_steps
    mul r1 a_max                       -- n * a_max
    st r1 temp
    cmpgt r0 temp                      -- v > n * a_max ?
    goto_ct "set_v"
    ld r1 v
    sub r1 a_max
    "set_v" @@ st r1 v

    -- speed check
    ld_i r0 0
    st r0 temp
    cmpeq r1 temp                     -- v == 0?
    goto_cf "inc_s"                   -- speed is non-zero: continue

    -- in case speed is 0, check distance covered:
    ld r0 s
    cmpeq r0 dist
    goto_cf "reaccelerate"
    halt                              -- we have reached our destination
    "reaccelerate" @@ ld r0 dist
    sub r0 s                          -- dist - s
    cmplt r0 a_max
    goto_ct "set_v2"
    ld r0 a_max
    "set_v2" @@ st r0 v

    "inc_s" @@ ld r0 s
    add r0 v
    st r0 s

    halt

initCtx :: Context
initCtx = MkContext
  { _pathCondition = SConst (CBool True)
  , _constraints =
    [ ("0 < a_max < 10",  (SGt a_max 0) &&& (SLt a_max 100))
    , ("0 < v_max < 100", (SGt v_max 0) &&& (SLt v_max 100))
    , ("0 < dist < 1000", (SGt dist 0) &&& (SLt dist 100))
    , ("0 < s < 100",     (SGt s 0) &&& (SLt s 100))
    , ("0 < v < v_max",   (SGt v 0) &&& (SLt v v_max))
    ]
  , _bindings = Map.fromList $ [ (IC, SConst 0)
                               , (IR, 0)
                               , (F Condition, SConst (CBool False))
                               , (F Halted, SConst (CBool False))
                               , (F Overflow, SConst (CBool False))
                               , (Addr 0, SAny "a_max")
                               , (Addr 1, SAny "v_max")
                               , (Addr 2, SAny "dist")
                               , (Addr 3, SAny "s")
                               , (Addr 4, SAny "v")
                               ] ++ mkProgram mc_loop
  , _solution = Nothing
  }
  where a_max = SAny "a_max"
        v_max = SAny "v_max"
        dist = SAny "dist"
        s = SAny "s"
        v = SAny "v"

theorem :: Symbolic (Trace Context)
theorem = do
  a_max <- forall "a_max"
  v_max <- forall "v_max"
  dist <- forall "dist"
  s <- forall "s"
  v <- forall "v"


  constrain ("0 < a_max < 10", const $ (SGt a_max 0) &&& (SLt a_max 100))
  constrain ("0 < v_max < 100", const $ (SGt v_max 0) &&& (SLt v_max 100))
  constrain ("0 < dist < 1000", const $ (SGt dist 0) &&& (SLt dist 100))
  constrain ("0 < s < 100", const $ (SGt s 0) &&& (SLt s 100))
  constrain ("0 < v < v_max", const $ (SGt v 0) &&& (SLt v v_max))

  let mem = mkMemory [(0, a_max), (1, v_max), (2, dist), (3, s), (4, v)]
  initialState <- boot mc_loop defaultRegisters mem defaultFlags

  liftIO (runModel 1000 initialState)


showContext :: Context -> String
showContext ctx =
  unlines [ "Path constraint: " <> show (_pathCondition ctx)
  , showKey ctx IC
  , showKey ctx IR
  , showKey ctx (F Condition)
  , showKey ctx (F Halted)
  , showKey ctx (Reg R0)
  , showKey ctx (Reg R1)
  , showKey ctx (Reg R2)
  , showKey ctx (Addr 3)
  , showKey ctx (Addr 4)
  ]


demo :: IO ()
demo = do
  tr <- runSymbolic theorem
  solved <- solveTrace (fst tr)
  -- let cs = fmap (\(Node _ s ctx) -> showContext ctx) (unTrace (fst tr))
  let z = fmap (\(Node _ ctx) -> showContext ctx) (unTrace solved)
  mapM putStrLn z

  -- let dataGraph =
  --       fromJust $ programDataGraph (assemble mc_loop)
  -- writeFile "/home/geo2a/Desktop/traces/graph_motor.dot" (drawGraph dataGraph)

  -- putStrLn ""
  -- let t = runModel 50 ctx
  --     tracePath = "/home/geo2a/Desktop/traces/trace_motor.html"
  -- writeTraceHtmlFile showContext tracePath t
  -- putStrLn $ "Wrote trace into file " <> tracePath

  -- debugConsole 10 ctx
  pure ()

---------------- Loop Body Analysis --------------------------------------------
-- | To check properties of programs, we take the following approach:
--   (1) Obtain a binary tree-shaped trace by /bounded/ symbolic execution
--   (2) Split the trace in linear paths (we do not share common prefixes now)
--   (3) Analyse every path separately:
--       1. Extract the interesting parts of the state from the /last/ node in
--           the path, i.e. symbolic expressions stored in
--           registers/memory/flags
--       2. Construct a symbolic expression representing the /property to check/,
--           which would involve the expression obtained in the previous step
--       3. Extract the /path constraints/ from the /last/ node in the path and
--           conjunct them.
--       4. Formulate the /preconditions/ of the program and conjunct them
--       5. To verify the property in the given path, check the following
--           formula for satisfiability:
--             preconditions /\ path constraints /\ ¬ property to check
--   (4) The property holds if, for every path, the solver returns
--       @Unsatisfiable@, i.e. there are no assignments of the variables which
--       satisfy the /negation/ of the property to check, considering the
--       preconditions and path constraints.
-- motorControlBodyExample :: Int -> IO ()
-- motorControlBodyExample steps = do
--     let a_max = SAny "a_max"
--         v_max = SAny "v_max"
--         dist  = SAny "dist"
--     -- ^ @a_max@, @v_max@ and @dist@ are the parameters of the algorithm.
--     -- To verify the loop invariant, we will need to check it for every
--     -- permitted value of @a_max@, @v_max@ and @dist@
--         s     = SAny "s"
--         v     = SAny "v"
--     -- ^ @s@ and @v@ are the internal state of the loop:
--     --   the distance travelled and the current velocity.
--     --   We universally quantify over @s@ and @v@ and thus checking the loop
--     --   invariant for every possible iterations of the loop.
--         mem = initialiseMemory [ (0, a_max)
--                                , (1, v_max)
--                                , (2, dist)
--                                , (3, s)
--                                , (4, v)
--                                ]
--         initialState = boot (assemble mc_loop) mem

--     -- Now we perform symbolic simulation with @runModel@ and obtain
--     -- a binary tree-shaped trace.
--     let trace = runModel steps initialState
--     -- and split the tree on paths
--     -- (at the moment, we do not share the common prefixes)
--         ps = paths (unTrace trace)
--     putStrLn $ "Non-trivial paths: " <> show (length ps)
--     -- ps' <- filterM (\p -> not <$> isDead preconditions p) ps
--     -- putStrLn $ "Reachable paths: " <> show (length ps')
--     -- putStrLn $ "Reachable paths: "   <> show (length ps')
--     putStrLn "--------------------------------------------------"

--     -- The following command will pretty-print the trace. Handle with care, and
--     -- use only with small amounts of @steps@, otherwise it's not likely to feet
--     -- in the screen space.
--     -- putStrLn $ renderTrace trace

--     -- Check the property for every path
--     mapM_ (processPath) (zip [1..] ps)
--     where
--         -- Formulate the preconditions that must hold at the start of every
--         -- iteration of the loop
--         preconditions :: State -> Sym Bool
--         preconditions initState =
--             let a_max = (Map.!) (memory initState) 0
--                 v_max = (Map.!) (memory initState) 1
--                 dist  = (Map.!) (memory initState) 2
--                 s     = (Map.!) (memory initState) 3
--                 v     = (Map.!) (memory initState) 4
--             in
--             -- @v@ must be in range [0, v_max]
--                 ((v `SGt` (SConst $ -1)) `SAnd` (v `SLt` (SAdd v_max (SConst 1))))
--                 `SAnd`
--                 -- @s@ must be in range [0, dist]
--                 ((s `SGt` (SConst $ -1)) `SAnd` (s `SLt` (SAdd dist (SConst 1))))
--                 `SAnd`
--                 -- @a_max@ must be <= v_max
--                 (SLt a_max (SAdd v_max (SConst 1)))
--                 `SAnd`
--                 (v_max `SGt` (SConst 0) `SAnd` (v_max `SLt` (SConst $ 30)))
--                 `SAnd`
--                 (a_max `SGt` (SConst 0) `SAnd` (a_max `SLt` (SConst $ 30)))

--         -- | Check that @v@ doesn't exceed @v_max@ at a certain state
--         invariant :: State -> Sym Bool
--         invariant state =
--             let v     = (Map.!) (memory state) 4
--                 v_max = (Map.!) (memory state) 1
--                 v_next = (Map.!) (memory state) 8
--             in SLt v (SAdd v_max (SConst 1))

--         processPath :: (Int, Path (Node State)) -> IO ()
--         processPath (pathId, path) = do
--             dead <- isDead preconditions path
--             if dead then pure () -- putStrLn "dead"
--             else do
--                 putStrLn $ "Path id: "       <> show      pathId
--                 putStrLn $ "Nodes in path: " <> show (length path)
--                 s <- solvePath path preconditions invariant (const (SConst True))
--                 print s
--                 -- s <- solvePath path
--                 -- let sbvVC = SMT.toSMT [symVC]
--                 -- satStart  <- getCPUTime
--                 -- satResult <- SBV.satWith SMT.prover sbvVC
--                 -- satFinish <- getCPUTime
--                 -- putStrLn $ "Find VC : "      <> show symVC
--                 -- putStrLn $ show satResult
--             putStrLn $ "--------------------------------------------"

-- | Generate a data-flow graph of the motor control program's loop body
--   and write it to an .dot file
--
--   It may be then converted to .svg with the following command:
--   dot -Tsvg motorControlLoop.dot -o motorControlLoop.svg
mtLoopGraph :: FilePath -> IO ()
mtLoopGraph fname =
    writeFile fname $
        drawGraph $ fromJust $ programDataGraph (assemble mc_loop)
