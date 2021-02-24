{-# LANGUAGE AllowAmbiguousTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Graph.BasicBlock
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
-- module ISA.Backend.Graph.BasicBlock (Block(..), basicBlocks, validateBlock) where
module ISA.Backend.Graph.BasicBlock where

import qualified Debug.Trace              as Debug

import           Control.Monad
import           Control.Selective
import           Data.Functor
import           Data.Int
import qualified Data.List                as List
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Ord
import           Polysemy
import           Polysemy.Error
import           Polysemy.State

import           ISA.Assembly
import           ISA.Backend.Dependencies
import           ISA.Backend.Simulation
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Instruction
import           ISA.Types.Prop
import           ISA.Types.ZeroOneTwo

-- | A basic block of a program is a sequence of instructions
--   that starts with a label, finishes with a branching instruction and
--   has no labels/branches in the body
data Block a = MkBlock { _entry   :: Address
                       , _body    :: [Instruction a]
                       , _exit    :: Address
                       , _targets :: ZeroOneTwo Address
                       }

instance Show a => Show (Block a) where
  show (MkBlock a is _ t) =
    unlines $ (map (\(a, i) -> show a <> ". " <> show i) . zip [a..] $ is) ++
      [ "Targets: " <> case t of
                         Zero    -> "⊥"
                         One n   -> show n
                         Two l r -> show l <> " " <> show r
      ]




instance Eq (Block a) where
  a == b = (_entry a) == (_entry b)

instance Ord (Block a) where
  compare a b = compare (_entry a) (_entry b)

-- | We really need to define custom effects here, but as a workaround,
--   let's create this orphan instance of 'Control.Selective' for @Sem@.
--   We need over approximation --> so let's use @selectA@ and execute both branches
instance Selective (Sem r) where
  select = selectA

-- | Calculate basic blocks of an assembly script
basicBlocks :: Script -> [Block (Data Int32)]
basicBlocks = map postprocessBlock . basicBlocksImpl . assemble

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
basicBlocksImpl src = simulate [] init $ do
  -- calculate leaders -- starts of the blocks,
  -- sort and consider only unique leaders to avoid spurious empty blocks
  ls <- List.nub . List.sort <$> leaders src
  -- calculate address ranges for each basic block: from a leader's address to
  -- its corresponding finishing instruction (the one before the next leader)
  let bodyRanges = (zip ls (tail ls ++ [fst (last src) + 1]))
  -- for every leader, build a basic block by computing targets of the last
  -- instruction
  forM bodyRanges $ \(entry, exit) -> do
    -- get the last instruction of the block from the source
    i <- maybe (throw $ missing (Prog entry)) pure $
      List.lookup entry src
    -- lookup the instructions of the body in the source
    let body = map (\a -> (a, fromJust $ List.lookup a src)) [entry..exit-1]
    case body of
      [] -> error $ "empty block starting at " <> show entry
      _ -> do
        targets <- computeTargets (last body)
        pure (MkBlock entry (map snd body) (fst (last body)) targets)
  where init = emptyCtx {_bindings = Map.fromList [(IC, 0), (F Condition, false)]}

-- | Compute target addresses of an instruction, if any.
--   * Conditional jumps have two targets
--   * Unconditional jumps have one target
--   * All other instructions don't alter control flow and thus have no targets
computeTargets :: ( Addressable a, Num a, Show a
                  , Monoid a, Integral a, Bounded a, Boolean a, TryOrd a)
  => (Address, Instruction a) -> Simulate a (ZeroOneTwo Address)
computeTargets (iAddr, i) = do
  let (Reads reads, _) = dependencies (instructionSemanticsS i)
  fetch >> execute >> incrementIC
  addrAfter <- toMemoryAddress <$> simulateRead IC
  case List.find (== IC) reads of
    Nothing -> pure $ One (iAddr + 1)
    Just _  -> case addrAfter of
      -- malformed program address -- create an exception for it instead of crashing
      Nothing -> error "basicBLocks: invalid program address reached"
      Just target ->
        case List.find (== F Condition) reads of
          Nothing -> pure $ One target
          Just _  -> pure $ Two (iAddr + 1) target
  where
    fetch = void $ simulateWrite IC (pure (fromMemoryAddress iAddr))
    execute =   void $ try (instructionSemanticsM i simulateRead simulateWrite)
    incrementIC = void $ simulateWrite IC ((+ 1) <$> simulateRead IC)

-- | Calculate leaders --- instructions that start basic blocks
--   See https://en.wikipedia.org/wiki/Basic_block
leaders :: forall a.
  ( Addressable a, Num a, Show a
  , Monoid a, Integral a, Bounded a, Boolean a, TryOrd a)
  => [(Address, Instruction a)] -> Simulate a [Address]
leaders [] = pure []
leaders (x:xs) =
  go [0] $ (x:xs)
  where
    go ls = \case
      [] -> pure ls
      ((a, i):ys) -> do
        let (Reads reads, _) = dependencies (instructionSemanticsS i)
        t <- computeTargets (a, i)
        case List.find (== IC) reads of
          Nothing -> go ls ys
          Just _ ->
            case t of
              Zero            -> go ls ys
              One target      -> go (target:ls) ys
              Two next target -> go (next:target:ls) ys

-- | Validate a basic block and remove spurious targets of @halt@'s
postprocessBlock :: Block a -> Block a
postprocessBlock b@(MkBlock a is e t) =
  case reverse is of
    [] -> error $ "empty block starting at " <> show a <> " with target " <> show t
    -- analyse the exit instruction
    ((Instruction i):_) -> case i of
      Halt -> (MkBlock a is e Zero)
      _    -> b
