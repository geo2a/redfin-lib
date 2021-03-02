{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Symbolic.SMT
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Semantics of symbolic expressions in terms of SMT
--
-----------------------------------------------------------------------------

module ISA.Types.Symbolic.SMT
    ( -- statistics after running symbolic execution with solving
      SymExecStats (..)

      -- get the list of free variables in a symbolic expression
    , findFreeVars, gatherFree

    -- declare variables/expressions as symbolic
    , createSym

      -- helper functions
    , conjoin

    , symAddress, symInt32, toSMT
    ) where

import           Data.Int
import qualified Data.Map.Strict            as Map
import qualified Data.SBV.Trans             as SBV
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time.Clock            (NominalDiffTime)
import           Data.Traversable
import           GHC.Stack
import           Prelude                    hiding (not)

import           ISA.Types
import           ISA.Types.Context          hiding (Context)
import qualified ISA.Types.Context          as ISA.Types
import           ISA.Types.SBV.SFunArray    (SFunArray)
import qualified ISA.Types.SBV.SFunArray    as SFunArray
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address

type Context = ISA.Types.Context Sym

data SymExecStats = MkSymExecStats { _timing :: NominalDiffTime }
  deriving Show

-- | Find all free symbolic variables (SAny) in the context
findFreeVars :: Context -> Set Sym
findFreeVars ctx =
  let fromBindings = mconcat . fmap gatherFree . Map.elems $ _bindings ctx
      fromConstraints = gatherFree . conjoin . map snd $ _constraints ctx
  in fromBindings <> fromConstraints

-- | Walk through a symbolic expression gathering up the free variables.
gatherFree :: Sym -> Set Sym
gatherFree c@(SAny _)   = Set.singleton c
gatherFree (SPointer p) = gatherFree p
gatherFree (SAdd l r)   = gatherFree l <> gatherFree r
gatherFree (SSub l r)   = gatherFree l <> gatherFree r
gatherFree (SMul l r)   = gatherFree l <> gatherFree r
gatherFree (SDiv l r)   = gatherFree l <> gatherFree r
gatherFree (SMod l r)   = gatherFree l <> gatherFree r
gatherFree (SAbs l)     = gatherFree l
gatherFree (SNot c)     = gatherFree c
gatherFree (SOr l r)    = gatherFree l <> gatherFree r
gatherFree (SAnd l r)   = gatherFree l <> gatherFree r
gatherFree (SEq l r)    = gatherFree l <> gatherFree r
gatherFree (SGt l r)    = gatherFree l <> gatherFree r
gatherFree (SLt l r)    = gatherFree l <> gatherFree r
gatherFree (SConst _)   = mempty

-- | Create existential SVals for each of SAny's in the input.
createSym :: (HasCallStack, SBV.MonadSymbolic m)
          => [Sym] -> m (Map.Map Text SBV.SInt32)
createSym cs = do
  pairs <- traverse createSymPair cs
  pure $ Map.fromList pairs
    where createSymPair (SAny name) = do
            v <- SBV.sInt32 (Text.unpack name)
            pure (name, v)
          createSymPair _ = error "Non-variable encountered."

-- | Convert a list of path constraints to a symbolic value the SMT solver can solve.
--   Each constraint in the list is conjoined with the others.
toSMT :: (HasCallStack, Applicative m)
      => SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> [Sym] -> m SBV.SBool
toSMT mem vars cs = do
  let smts = process $ map (symBool mem vars) cs
  pure $ conjoinSBV smts

  where process [] = []
        process (x:xs) =
          case x of
            Nothing -> process xs
            Just y  -> y:process xs

-- symPointer (Map.Map Text SBV.SInt32) -> Sym -> Maybe SBV.SInt32
-- symPointer vars = \case
--   SPointer p -> symInt32 p
--   _ -> Nothing

symInt32 :: SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> Sym -> Maybe SBV.SInt32
symInt32 mem vars = \case
  (SConst (CInt32 i)) -> Just (SBV.literal i)
  SAny x              -> Map.lookup x vars
  SPointer p          -> symAddress vars (MkAddress (Right p)) >>=
                         Just . SFunArray.readArray mem
  SAdd l r            -> (+) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SSub l r            -> (-) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SMul l r            -> (*) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SDiv l r            -> (SBV.sDiv) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SMod l r            -> (SBV.sMod) <$> symInt32 mem vars l <*> symInt32 mem vars r
  SAbs x              -> abs <$> symInt32 mem vars x
  _                   -> Nothing

symBool :: SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> Sym -> Maybe SBV.SBool
symBool mem vars = \case
  (SConst (CBool b)) -> Just $ SBV.literal b
  (SConst _        ) -> Nothing
  SAny x             -> Nothing
  (SEq l r)          -> (SBV..==) <$> symInt32 mem vars l <*> symInt32 mem vars r
  (SGt  l r)         -> (SBV..>)  <$> symInt32 mem vars l <*> symInt32 mem vars r
  (SLt  l r)         -> (SBV..<)  <$> symInt32 mem vars l <*> symInt32 mem vars r
  (SAnd l r)         -> (SBV..&&) <$> symBool mem vars l  <*> symBool mem vars r
  (SOr  l r)         -> (SBV..||) <$> symBool mem vars l  <*> symBool mem vars r
  (SNot x  )         -> SBV.sNot  <$> symBool mem vars x
  _                  -> Nothing

-- | Interpret a  symbolic memory address into an SBV symbolic integer
--   We do not allow nested pointers
symAddress :: Map.Map Text SBV.SInt32 -> Address -> Maybe SBV.SInt32
symAddress vars = \case
  MkAddress (Left (CAddress concrete)) -> Just (SBV.literal (fromIntegral concrete))
  MkAddress (Right sym) ->
    -- no nested pointers: hence the empty memory
    symInt32 (SFunArray.sListArray 0 []) vars sym

conjoinSBV :: [SBV.SBool] -> SBV.SBool
conjoinSBV = foldr (\x y -> (SBV..&&) x y) (SBV.sTrue)
------------------------ -----------------------------------------------------
