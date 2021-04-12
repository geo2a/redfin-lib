{- |
 Module     : ISA.Types.SBV
 Copyright  : (c) Levent Erkok, Georgy Lukyanov 2017-2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Simplified datatypes from SBV and missing instances
-}
module ISA.Types.SBV where

import Data.Aeson (
    FromJSON,
    ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
 )
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics

-- | A simplified version of SBV's @SMTModel datatype
newtype SMTModel = MkSMTModel {modelAssocs :: Map Text Int32}
    deriving (Generic)

instance Show SMTModel where
    show (MkSMTModel m) = unlines . map show $ Map.assocs m

instance ToJSON SMTModel where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SMTModel

-- | A simplified version of SBV's @SMTResult datatype
data SMTResult
    = Unsatisfiable
    | Satisfiable SMTModel
    deriving (Generic, Show)

instance ToJSON SMTResult where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SMTResult

isUnsat :: SMTResult -> Bool
isUnsat = \case
    Unsatisfiable -> True
    _ -> False

isSat :: SMTResult -> Bool
isSat = \case
    Satisfiable _ -> True
    _ -> False

-- | Extract a model of the argument is satisfiable
getModel :: SMTResult -> Maybe SMTModel
getModel = \case
    Unsatisfiable -> Nothing
    Satisfiable m -> Just m
