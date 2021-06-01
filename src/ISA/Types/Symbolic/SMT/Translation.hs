{- |
 Module     : ISA.Types.Symbolic.SMT.Translation
 Copyright  : (c) Georgy Lukyanov 2019
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Translation of symbolic expressions ('ISA.Types.Symbolic.Sym') to SMTlib via SBV
-}
module ISA.Types.Symbolic.SMT.Translation (
    findFreeVars,
    createSym,
    toSMT,
    symbolicMemory,
) where

import Data.Bifunctor
import Data.Int
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.SBV.Trans as SBV
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import ISA.Types
import ISA.Types.Context
import ISA.Types.SBV.SFunArray (SFunArray)
import qualified ISA.Types.SBV.SFunArray as SFunArray
import ISA.Types.Symbolic
import ISA.Types.Symbolic.Address

-- | Find all free symbolic variables 'SAny' in the context
findFreeVars :: Context Sym -> Set Sym
findFreeVars ctx =
    let fromBindings = mconcat . fmap gatherFree . Map.elems $ _bindings ctx
        fromStore = mconcat . fmap gatherFree . Map.elems $ _store ctx
        fromConstraints = gatherFree . conjoin . map snd $ _constraints ctx
     in fromBindings <> fromStore <> fromConstraints

-- | Walk through a symbolic expression gathering up the free variables.
gatherFree :: Sym -> Set Sym
gatherFree = \case
    c@(SAny _) -> Set.singleton c
    (SPointer p) -> gatherFree p
    (SIte i t e) -> gatherFree i <> gatherFree t <> gatherFree e
    (SAdd l r) -> gatherFree l <> gatherFree r
    (SSub l r) -> gatherFree l <> gatherFree r
    (SMul l r) -> gatherFree l <> gatherFree r
    (SDiv l r) -> gatherFree l <> gatherFree r
    (SMod l r) -> gatherFree l <> gatherFree r
    (SAbs l) -> gatherFree l
    (SNot c) -> gatherFree c
    (SOr l r) -> gatherFree l <> gatherFree r
    (SAnd l r) -> gatherFree l <> gatherFree r
    (SEq l r) -> gatherFree l <> gatherFree r
    (SGt l r) -> gatherFree l <> gatherFree r
    (SLt l r) -> gatherFree l <> gatherFree r
    (SConst _) -> mempty

-- symIte ::
--     SFunArray Int32 Int32 ->
--     Map.Map Text SBV.SInt32 ->
--     Sym ->
--     Maybe SBV.SInt32
-- symIte mem vars = \case

--     _ -> Nothing

-- | Convert a symbolic expression into an SBV 'Int32'
symInt32 :: SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> Sym -> Maybe SBV.SInt32
symInt32 mem vars = \case
    (SConst (CInt32 i)) -> Just (SBV.literal i)
    SAny x -> Map.lookup x vars
    SIte i t e ->
        SBV.ite <$> (symBool mem vars i) <*> symInt32 mem vars t <*> symInt32 mem vars e
    SPointer p -> do
        v <-
            symAddress vars (MkAddress (Right p))
                >>= Just . SFunArray.readArray mem
        pure v
    SAdd l r -> (+) <$> symInt32 mem vars l <*> symInt32 mem vars r
    SSub l r -> (-) <$> symInt32 mem vars l <*> symInt32 mem vars r
    SMul l r -> (*) <$> symInt32 mem vars l <*> symInt32 mem vars r
    SDiv l r -> (SBV.sDiv) <$> symInt32 mem vars l <*> symInt32 mem vars r
    SMod l r -> (SBV.sMod) <$> symInt32 mem vars l <*> symInt32 mem vars r
    SAbs x -> abs <$> symInt32 mem vars x
    _ -> Nothing

-- | Convert a symbolic expression into an SBV 'Bool'
symBool :: SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> Sym -> Maybe SBV.SBool
symBool mem vars = \case
    (SConst (CBool b)) -> Just $ SBV.literal b
    (SConst _) -> Nothing
    SAny _ -> Nothing
    SIte i t e ->
        SBV.ite <$> (symBool mem vars i) <*> symBool mem vars t <*> symBool mem vars e
    (SEq l r) -> (SBV..==) <$> symInt32 mem vars l <*> symInt32 mem vars r
    (SGt l r) -> (SBV..>) <$> symInt32 mem vars l <*> symInt32 mem vars r
    (SLt l r) -> (SBV..<) <$> symInt32 mem vars l <*> symInt32 mem vars r
    (SAnd l r) -> (SBV..&&) <$> symBool mem vars l <*> symBool mem vars r
    (SOr l r) -> (SBV..||) <$> symBool mem vars l <*> symBool mem vars r
    (SNot x) -> SBV.sNot <$> symBool mem vars x
    _ -> Nothing

{- | Interpret a  symbolic memory address into an SBV symbolic integer
   We do not allow nested pointers
-}
symAddress :: Map.Map Text SBV.SInt32 -> Address -> Maybe SBV.SInt32
symAddress vars = \case
    MkAddress (Left (CAddress concrete)) -> Just (SBV.literal (fromIntegral concrete))
    MkAddress (Right sym) ->
        symInt32 (SFunArray.sListArray 0 []) vars sym

-- | Create a symbolic array representing memory with possibly symbolic addresses
symbolicMemory :: Map Text SBV.SInt32 -> [(Address, Sym)] -> SFunArray SBV.Int32 SBV.Int32
symbolicMemory vars mem =
    SFunArray.sListArray 0
        . map (second (maybe 0 id . symInt32 (SFunArray.sListArray 0 []) vars))
        . map (first (maybe 0 id . symAddress vars))
        $ mem

-- | Create existential symbolic variables for each of 'SAny''s in the input.
createSym :: SBV.MonadSymbolic m => [Sym] -> m (Map.Map Text SBV.SInt32)
createSym cs = do
    pairs <- traverse createSymPair cs
    pure $ Map.fromList pairs
  where
    createSymPair (SAny name) = do
        v <- SBV.sInt32 (Text.unpack name)
        pure (name, v)
    createSymPair _ = error "Non-variable encountered."

{- | Convert a list of path constraints to a symbolic value the SMT solver can solve.
   Each constraint in the list is conjoined with the others.
-}
toSMT :: SFunArray Int32 Int32 -> Map.Map Text SBV.SInt32 -> Sym -> SBV.SBool
toSMT mem vars expr =
    case symBool mem vars expr of
        Nothing -> SBV.sFalse
        Just e -> e
