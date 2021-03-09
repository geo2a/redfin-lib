-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Symbolic.ZipperEngine.Run
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Symbolic simulation over a zipper-focused binary tree
-----------------------------------------------------------------------------

module ISA.Backend.Symbolic.Zipper.Run (runModel, runModelImpl, continueModel) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Reader
import           Control.Monad.Reader.Class   ()
import           Control.Monad.State.Class
import           Data.Functor                 (void)
import qualified Data.IntMap                  as IntMap
import           Data.Maybe
import qualified Data.SBV.Trans               as SBV
import qualified Data.SBV.Trans.Control       as SBV
import qualified Data.Set                     as Set
import           Prelude                      hiding (log, not)

import           ISA.Backend.Dependencies
import           ISA.Backend.Symbolic.Zipper
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Context            hiding (Context)
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Key
import           ISA.Types.Prop
import           ISA.Types.SBV
import qualified ISA.Types.SBV.SFunArray      as SFunArray
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Tree               hiding (down, left, right, top,
                                               up)
import           ISA.Types.ZeroOneTwo

-- | Fetching an instruction is a Monadic operation. It is possible
--   (and natural) to implement in terms of @FS Key Monad Value@, but
--   for now we'll stick with this concrete implementation in the
--   @Engine@ monad.
fetchInstruction :: Engine (Data Sym)
fetchInstruction =
  readKey IC >>= \(MkData x) ->
    case (toAddress . MkData =<< (getValue (simplify (Just 100) x))) of
      Just ic -> do
        i <- readKey (Prog ic)
        writeKey IR (pure i)
      Nothing ->
        error $ "Engine.fetchInstruction: symbolic or malformed instruction counter "

incrementInstructionCounter :: Engine ()
incrementInstructionCounter = do
  void $ writeKey IC ((+ 1) <$> readKey IC)

readInstructionRegister :: Engine InstructionCode
readInstructionRegister =  do
  x <- (fmap toInstructionCode) <$> readKey IR
  case x of
    (MkData (Right instr)) -> pure instr
    (MkData (Left sym)) -> error $ "Engine.readInstructionRegister: " <>
                           "symbolic instruction code encountered " <> show sym

-- | Perform one step of symbolic execution
step :: Engine (ZeroOneTwo Context)
step = do
  void $ fetchInstruction
  incrementInstructionCounter
  ic <- readInstructionRegister
  -- decode the instruction in the focused state
  let i = maybe (errorIR ic) id (decode ic)
  -- analyse the instruction's dependencies via its selective semantics
  -- and create children states (if any)
  choice <- analyse i
  -- execute the instruction's monadic semantics in the children states
  execute (void $ instructionSemanticsM i readKey writeKey) choice
  where
    -- | Error out in case of an unknown/symbolic instruction code
    --   in the instruction register
    errorIR icode = error $ "Engine.step: " <>
                            "unknown instruction with code " <> show icode

execute :: Engine () -> ZeroOneTwo Context -> Engine (ZeroOneTwo Context)
execute todo = \case
  Zero -> getFocused >>= \end -> from end todo >> pure Zero
  One ctx -> do
    (reachable, alteredCtx) <- liftIO . SBV.runSMT . reach =<< from ctx todo
    if reachable then pure $ One alteredCtx
                 else pure Zero
  Two ctx1 ctx2 -> do
    r1 <- liftIO . SBV.runSMT . reach =<< from ctx1 todo
    r2 <- liftIO . SBV.runSMT . reach =<< from ctx2 todo
    case (r1, r2) of
      -- both children are reachable
      ((True, ctx1'), (True, ctx2')) -> pure $ Two ctx1' ctx2'
      -- one is reachable
      ((False, _)   , (True, ctx2')) -> pure $ One ctx2'
      ((True, ctx1') , (False, _))   -> pure $ One ctx1'
      -- none is reachable!
      ((False, _)   , (False, _))    -> pure Zero

  where
    from :: Context -> Engine a -> Engine Context
    from ctx doSomething = putFocused ctx *> doSomething *> getFocused

    -- | Check satisfiability of a Context's path condition under its constraints
    reach :: Context -> SBV.Symbolic (Bool, Context)
    reach ctx = do
      let freeVars = findFreeVars (_unData <$> ctx)
      vars <- createSym (Set.toList freeVars)
      constrs <- toSMT (SFunArray.sListArray 0 [])
                       vars ((_unData $ _pathCondition ctx)
                             :(map (_unData . snd) (_constraints ctx)))
      SBV.query $ do
        SBV.constrain constrs
        SBV.checkSat >>= \case
            SBV.Unk -> pure $ (False, ctx { _solution = Nothing })
            _ -> SBV.getSMTResult >>= \case
              (SBV.Satisfiable _ _) -> do
                values <- traverse SBV.getValue vars
                pure $ (True, ctx { _solution = (Just . Satisfiable . MkSMTModel $ values) })
              (SBV.Unsatisfiable _ _) ->
                pure $ (False, ctx { _solution = Just $ Unsatisfiable })
              _ -> error "not implemented"

-- | Decide how many children states there will be according to the dependencies
--   of an instruction
analyse :: Instruction (Data Sym) -> Engine (ZeroOneTwo Context)
analyse i =
  let (Reads rs, Writes ws) = dependencies (instructionSemanticsS i)
  in if | F Halted `elem` ws -> pure Zero
        | IC `elem` ws ->
            if | F Condition `elem` rs -> branchOn Condition <$> getFocused
               | otherwise             -> One <$> getFocused
        | otherwise -> One <$> getFocused

-- | Create zero, one or two successor states depending on the state of a flag;
--   Stop by returning @Zero@ if the flag is not bound.
branchOn :: Flag -> Context -> ZeroOneTwo Context
branchOn f s = case f of
  Halted -> Zero
  Condition ->
    case getBinding (F Condition) s of
      Nothing -> Zero
      Just cond ->
        let yes = putBinding (F Condition) true $
                  s {_pathCondition = cond &&& (_pathCondition s)}
            no  = putBinding (F Condition) false $
                  s {_pathCondition = not cond &&& (_pathCondition s)}
        in Two yes no
  Overflow -> One s

-- | Run symbolic simulation for a number of steps
runModel :: Int -> Context -> IO Trace
runModel steps init = execEngine (runModelImpl steps) init

runModelImpl :: Int -> Engine ()
runModelImpl steps = do
  if | steps == 0 -> pure ()
--     | steps == 1 -> void step
     | otherwise -> do
       -- perform a step originating in the state (n, ctx)
       before <- getFocused
       choice <- step
       after <- getFocused
       -- add one or two children at the focus point
       putFocused before
       growTrace choice
       case choice of
         Zero -> do
           -- no reachable children! seal the branch
           growTrace (One before)
           down
           putFocused after
           pure ()
         (One _) -> do
          -- go down the tree trunk and continue
          down
          runModelImpl (steps - 1)
         (Two _ _) -> do
           branch <- get
           -- go into the left subtree
           leftBottom <- left *> runModelImpl (steps - 1) *> get
           -- backtrack to the branch and go into the right subtree
           wayUp branch leftBottom >> right >> runModelImpl (steps -1)
  where
    wayUp :: Loc Int () -> Loc Int () -> Engine ()
    wayUp there = go
      where go here = do
             case locKey here == locKey there of
               True  -> put here
               False -> up >> get >>= go

-- | Continue execution from a specific state
continueModel :: Int -> Int -> Engine ()
continueModel state steps = do
  env <- ask
  trace <- liftIO $ readTVarIO (_trace env)
  case IntMap.lookup state (_states trace) of
    Nothing -> error $ "undefined states with id " <> show state
    Just ctx -> do
      putFocused ctx
      runModelImpl steps
