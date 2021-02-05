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

    , toSMT
    ) where

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

import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context

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
gatherFree c@(SAny _) = Set.singleton c
gatherFree (SAdd l r) = gatherFree l <> gatherFree r
gatherFree (SSub l r) = gatherFree l <> gatherFree r
gatherFree (SMul l r) = gatherFree l <> gatherFree r
gatherFree (SDiv l r) = gatherFree l <> gatherFree r
gatherFree (SMod l r) = gatherFree l <> gatherFree r
gatherFree (SAbs l)   = gatherFree l
gatherFree (SNot c)   = gatherFree c
gatherFree (SOr l r)  = gatherFree l <> gatherFree r
gatherFree (SAnd l r) = gatherFree l <> gatherFree r
gatherFree (SEq l r)  = gatherFree l <> gatherFree r
gatherFree (SGt l r)  = gatherFree l <> gatherFree r
gatherFree (SLt l r)  = gatherFree l <> gatherFree r
gatherFree (SConst _) = mempty

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
      => Map.Map Text SBV.SInt32 -> [Sym] -> m SBV.SBool
toSMT vars cs = do
  let smts = process $ map (symBool vars) cs
  pure $ conjoinSBV smts

  where process [] = []
        process (x:xs) =
          case x of
            Nothing -> process xs
            Just y  -> y:process xs

symInt32 :: (Map.Map Text SBV.SInt32) -> Sym -> Maybe SBV.SInt32
symInt32 vars = \case
  (SConst (CInt32 i)) -> Just (SBV.literal i)
  SAny x              -> Map.lookup x vars
  SAdd l r            -> (+) <$> symInt32 vars l <*> symInt32 vars r
  SSub l r            -> (-) <$> symInt32 vars l <*> symInt32 vars r
  SMul l r            -> (*) <$> symInt32 vars l <*> symInt32 vars r
  SDiv l r            -> (SBV.sDiv) <$> symInt32 vars l <*> symInt32 vars r
  SMod l r            -> (SBV.sMod) <$> symInt32 vars l <*> symInt32 vars r
  SAbs x              -> abs <$> symInt32 vars x
  _                   -> Nothing

symBool :: (Map.Map Text SBV.SInt32) -> Sym -> Maybe SBV.SBool
symBool vars = \case
  (SConst (CBool b)) -> Just $ SBV.literal b
  (SConst _        ) -> Nothing
  SAny x             -> Nothing
  (SEq l r)          -> (SBV..==) <$> symInt32 vars l <*> symInt32 vars r
  (SGt  l r)         -> (SBV..>)  <$> symInt32 vars l <*> symInt32 vars r
  (SLt  l r)         -> (SBV..<)  <$> symInt32 vars l <*> symInt32 vars r
  (SAnd l r)         -> (SBV..&&) <$> symBool vars l  <*> symBool vars r
  (SOr  l r)         -> (SBV..||) <$> symBool vars l  <*> symBool vars r
  (SNot x  )         -> SBV.sNot  <$> symBool vars x
  _ -> Nothing

conjoinSBV :: [SBV.SBool] -> SBV.SBool
conjoinSBV = foldr (\x y -> (SBV..&&) x y) (SBV.sTrue)
------------------------ -----------------------------------------------------
