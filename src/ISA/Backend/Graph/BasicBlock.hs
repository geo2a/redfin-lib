{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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
module ISA.Backend.Graph.BasicBlock (Block(..), basicBlocks, validateBlock) where

import qualified Debug.Trace            as Debug

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
import           ISA.Types.Prop

-- | A basic block of a program is a sequence of instructions
--   that starts with a label, finishes with a branching instruction and
--   has no labels/branches in the body
data Block a = MkBlock { _entry   :: Address
                       , _body    :: [Instruction a]
                       , _targets :: Maybe (Address, Address)
                       }

instance Show a => Show (Block a) where
  show (MkBlock a is t) =
    unlines $ (map (\(a, i) -> show a <> ". " <> show i) . zip [a..] $ is) ++
            [ "Targets: " <> case t of
                               Nothing     -> "⊥"
                               Just (l, r) -> show l <> " " <> show r
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
  map postprocessBlock .
  run .
  fmap (either (const []) id) . runError @(Missing Key) .
  fmap (either (const []) id) . runError @(Ignored Key) .
  evalState initialState .
  go 0 [] []
  where
    go entry body bs = \case
      [] -> pure (reverse bs)
      ((a, i):xs) -> do
        -- set IC into the current address, essentially emulating instruction fetching
        controlFlowWrite IC (pure (fromMemoryAddress a))
        -- simulate semantics, together with post-incrementing IC
        s <- try @(Ignored Key) (instructionSemanticsM i controlFlowRead controlFlowWrite >> incrementIC)
        -- increment IC
        case toMemoryAddress <$> s of
          -- read of a non-IC key is ignored
          (Left _) -> go entry (i:body) bs xs
          -- invalid values of instruction counter fail the algorithm
          (Right Nothing) -> pure []
          -- a successful read means a jump was executed: start new block
          (Right (Just target)) ->
            let new = MkBlock entry (reverse $ i:body) (Just (a + 1, target))
            in go (a + 1) [] (new:bs) xs

    incrementIC =
      controlFlowWrite IC ((+ 1) <$> controlFlowRead IC)

    initialState = emptyCtx {_bindings = Map.fromList [ (IC, 0)
                                                      , (F Condition, false)
                                                      , (F Halted, false)
                                                      ]}

-- | Validate a basic block and remove spurious targets of @halt@'s
postprocessBlock :: Block a -> Block a
postprocessBlock b@(MkBlock a is t) =
  case reverse is of
    [] -> error $ "empty block starting at " <> show a <> " with target " <> show t
    -- analyse the exit instruction
    ((Instruction i):_) -> case i of
      Halt -> (MkBlock a is Nothing)
      _    -> b

-- | Simulate reads on control-flow related keys:
--   instruction counter, Halted flag and Condition flag,
--   ignoring everything else
controlFlowRead :: ( Member (State (Context a)) r
           , Member (Error (Missing Key)) r
           , Member (Error (Ignored Key)) r
           ) => Key -> Sem r a
controlFlowRead key = case key of
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
validateBlock (MkBlock _ is _) = case reverse is of
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
