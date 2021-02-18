{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Graph
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Calculate basics blocks of a program and arrange them into a control-flow graph
--
-- We demonstrate a technique of "forgetting" most of an existing read-write semantics
-- and focusing on two specific aspects of it: how the instruction counter and the
-- halting flag are modified. These are the keys we need to build basic blocks
-----------------------------------------------------------------------------
module ISA.Backend.CFG (basicBlocks, validateBlock) where

import           Control.Selective
import           Data.Int
import qualified Data.Map               as Map
import           Polysemy
import           Polysemy.Error
import           Polysemy.State

import           ISA.Assembly
import           ISA.Backend.Simulation
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Instruction


-- | A basic block of a program is a sequence of instructions
--   that starts with a label, finishes with a branching instruction and
--   has no labels/branches in the body
data Block a = MkBlock { _entry :: Address
                       , _body  :: [Instruction a]
                       } deriving Show

instance Selective (Sem r) where
  select = selectM

-- | Calculate basic blocks of an assembly script
basicBlocks :: Script -> [Block (Data Int32)]
basicBlocks = basicBlocksImpl . assemble

-- | For calculation of basic blocks we are interested only in changes in
--   the 'ISA.Types.Key.IC' (instruction counter) key, thus we produce the following
--   interpretation of the semantics:
--   * Try simulating the semantics via 'ISA.Backend.Simulation',
--     catching the 'ISA.Backend.Simulation.Missing' exception
--   * Speculatively convert the result into 'ISA.Types.Address'
--   * Since we are simulating with an empty initial context, into
--     which we only place a binding for 'ISA.Types.Key.IC', a read
--     of any other flag will throw an exception, which will be cough,
--     while the successful reads will produce the values of the instruction
--     counter and translated to addresses.
--
--   See 'ISA.Backend.CFG.controlFlowRead' and 'ISA.Backend.CFG.controlFlowRead'
--   for further details
basicBlocksImpl ::
  ( Addressable a, Num a, Show a
  , Monoid a, Integral a, Bounded a, Boolean a, TryOrd a)
  => [(Address, Instruction a)] -> [Block a]
basicBlocksImpl =
  run . evalState initialState .
  fmap (either (const []) id) . runError @(Missing Key) .
  fmap (either (const []) id) . runError @(Ignored Key).
  go 0 [] []
  where
    go entry body bs = \case
      [] -> pure (reverse bs)
      ((a, i):xs) -> do
        -- simulate semantics
        s <- try @(Ignored Key) (instructionSemanticsM i controlFlowRead controlFlowWrite)
        -- increment IC
        _ <- incrementIC
        case toMemoryAddress <$> s of
          -- read of a non-IC key is ignored
          (Left _) -> go entry (i:body) bs xs
          -- invalid values of instruction counter fail the algorithm
          (Right Nothing) -> pure []
          -- a successful read means a jump was executed: start new block
          (Right (Just _ )) -> go (a + 1) [] ((MkBlock entry (reverse $ i:body)):bs) xs

    incrementIC =
      simulateWrite IC ((+ 1) <$> simulateRead IC)

    initialState = emptyCtx {_bindings = Map.fromList [ (IC, 0)
                                                      , (F Condition, false)
                                                      , (F Halted, false)
                                                      ]}

-- | Simulate reads on control-flow related keys:
--   instruction counter, Halted flag and Condition flag,
--   ignoring everything else
controlFlowRead :: ( Member (State (Context a)) r
           , Member (Error (Missing Key)) r
           , Member (Error (Ignored Key)) r
           ) => Key -> Sem r a
controlFlowRead = \case
  F Halted    -> simulateRead (F Halted)
  F Condition -> simulateRead (F Condition)
  IC          -> simulateRead IC
  k           -> throw (ignore k)

-- | Simulate writes on control-flow related keys:
--   instruction counter, Halted flag and Condition flag,
--   ignoring everything else
controlFlowWrite :: ( Member (State (Context a)) r
            , Member (Error (Ignored Key)) r
            , Member (Error (Missing Key)) r
            ) => Key -> Sem r a -> Sem r a
controlFlowWrite key fv = do
  (flip catch) (\(MkMissing k) -> throw (ignore k)) $
    case key of
      F Halted    -> simulateWrite (F Halted) fv
      F Condition -> simulateWrite (F Condition) fv
      IC          -> simulateWrite IC fv
      k           -> throw (ignore k)


-- | Check that a block is indeed basic, i.e. it is non-empty
--   it finishes with a jump of halt and there ary no jumps or
--   halts in the middle
validateBlock :: Block a -> Bool
validateBlock (MkBlock _ is) = case reverse is of
  [] -> False
  (x:xs) -> isControlFlow x &&
            null (filter isControlFlow xs)
  where
    isControlFlow :: Instruction a -> Bool
    isControlFlow (Instruction i)= case i of
      Halt     -> True
      Jump _   -> True
      JumpCt _ -> True
      JumpCf _ -> True
      _        -> False
