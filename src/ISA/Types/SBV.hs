{-# OPTIONS_GHC -Wno-orphans #-}

-- | Simplified datatypes from SBV and missing instances
module ISA.Types.SBV where

import Control.Selective
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
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBV
import qualified Data.SBV.Internals as SBV
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

instance ToJSON SMTResult where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SMTResult

deriving instance Generic SBV.AlgRealPoly
instance ToJSON SBV.AlgRealPoly where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SBV.AlgRealPoly
deriving instance Generic (SBV.RealPoint Rational)
instance ToJSON (SBV.RealPoint Rational) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (SBV.RealPoint Rational)
deriving instance Generic SBV.AlgReal
instance ToJSON SBV.AlgReal where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SBV.AlgReal
deriving instance Generic (SBV.RCSet SBV.CVal)
instance ToJSON (SBV.RCSet SBV.CVal) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (SBV.RCSet SBV.CVal)
deriving instance Generic SBV.CVal
instance ToJSON SBV.CVal where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SBV.CVal
deriving instance Generic SBV.Kind
instance ToJSON SBV.Kind where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SBV.Kind
deriving instance Generic SBV.CV
instance ToJSON SBV.CV where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SBV.CV

--------------------------------------------------------------------------------
