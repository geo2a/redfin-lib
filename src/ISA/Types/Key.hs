module ISA.Types.Key
    ( -- ** Abstraction over possible locations in the ISA
      Key(..), keyTag )
    where

import           Control.Monad
import           Data.Aeson                 hiding (Value)
import           Data.Aeson.Types           hiding (Value)
import           Data.Bool
import           Data.List                  (isInfixOf, isPrefixOf)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as Text
import           GHC.Generics
import           Text.Read                  (readMaybe)

import           ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address

-- | Abstraction over possible locations in the ISA
data Key where
  Reg :: Register -> Key
  -- ^ data register
  Addr :: Address -> Key
  -- ^ memory cell
  F :: Flag -> Key
  -- ^ flag, a special boolean register
  IC :: Key
  -- ^ instruction counter
  IR :: Key
  -- ^ instruction register
  Prog :: Address -> Key
  -- ^ program address

-- -- | Parse key heuristically: we only need to be able to parse
-- --   registers, flags, addresses and IR. If first three options
-- --   fail it should be the IR.
-- parseKey :: String -> Maybe Key
-- parseKey key =
--    getFirst . mconcat . map First $ [ Reg  <$> readMaybe key
--                                     , F    <$> readMaybe key
--                                     , Addr <$> readMaybe key
--                                     , join $ bool Nothing (Just IR) <$>
--                                     (isInfixOf <$> Just "IR" <*> Just key)
--                                     , join $ bool Nothing (Just (Prog 0)) <$>
--                                     (isPrefixOf <$> Just "PROG" <*> Just key)
--                                     , join $ bool Nothing (Just IC) <$>
--                                     (isInfixOf <$> Just "IC" <*> Just key)
--                                     ]
deriving instance Eq Key
deriving instance Ord Key
deriving instance Generic Key

instance ToJSON Key where
  toEncoding = genericToEncoding defaultOptions
instance ToJSONKey Key where
  toJSONKey = toJSONKeyText (Text.pack . show)
-- instance FromJSON Key where
-- -- instance FromJSONKey Key where
-- instance FromJSONKey Key where
--   fromJSONKey = FromJSONKeyTextParser $ \t -> case parseKey (Text.unpack t) of
--     Just k  -> pure k
--     Nothing -> fail ("Invalid key: " ++ show t)

-- instance FromJSONKey Key where
--   fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

keyTag :: Key -> String
keyTag = \case
  Reg  _ -> "register"
  Addr _ -> "address"
  F    _ -> "flag"
  IC     -> "instruction-counter"
  IR     -> "instruction-register"
  Prog _ -> "program-address"

instance Show Key where
    show = \case
        Reg  reg  -> show reg
        Addr addr -> show addr
        F    flag -> show flag
        IC        -> "IC"
        IR        -> "IR"
        Prog addr -> "PROG " <> show addr
