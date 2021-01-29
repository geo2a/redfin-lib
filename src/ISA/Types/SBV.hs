-- | Simplified datatypes from SBV and missing instances
module ISA.Types.SBV where

import           Data.Aeson         (FromJSON, ToJSON, defaultOptions,
                                     genericToEncoding, toEncoding)
import qualified Data.SBV           as SBV
import qualified Data.SBV.Internals as SBV
import           GHC.Generics

-- | A simplified version of SBV's @SMTModel datatype
newtype SMTModel = MkSMTModel { modelAssocs :: [(String, SBV.CV)] }
  deriving (Generic, Show)

instance ToJSON SMTModel where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SMTModel

-- | A simplified version of SBV's @SMTResult datatype
data SMTResult = Unsatisfiable
               | Satisfiable SMTModel
               deriving (Generic, Show)

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
