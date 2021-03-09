{-# LANGUAGE DeriveAnyClass #-}
module ISA.Types.Key
    ( -- ** Abstraction over possible locations in the ISA
      Key(..), parseKey, keyTag )
    where

import qualified Data.Aeson                 as Aeson
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           GHC.Generics               (Generic)
import           Text.Megaparsec


import           ISA.Types
import           ISA.Types.Parser
import           ISA.Types.Symbolic.Address
import           ISA.Types.Symbolic.Parser

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

deriving instance Eq Key
deriving instance Ord Key
deriving instance Generic Key

deriving instance Aeson.ToJSON Key
deriving instance Aeson.FromJSON Key
deriving instance Aeson.ToJSONKey Key

instance Aeson.FromJSONKey Key where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ \t -> case parseKey t of
    Right k  -> pure k
    Left err -> fail ("Invalid key: " <> Text.unpack err)

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


parseKey :: Text -> Either Text Key
parseKey = either (Left . Text.pack . errorBundlePretty) (Right . id)
         . parse pKey ""

pKey :: Parser Key
pKey =  Reg  <$> pReg
    <|> Addr <$> pAddress
    <|> F    <$> pFlag
    <|> (IC <$ symbol "IC" <?> "instruction counter key label")
    <|> (IR <$ symbol "IR" <?> "instruction register key label")
    <|> (Prog <$> pAddress <?> "program address key")

pReg :: Parser Register
pReg = (symbol "R0" *> pure R0)
   <|> (symbol "R1" *> pure R1)
   <|> (symbol "R2" *> pure R2)
   <|> (symbol "R3" *> pure R3)
   <?> "register"

pFlag :: Parser Flag
pFlag = (symbol "Halted" *> pure Halted)
    <|> (symbol "Overflow " *> pure Overflow)
    <?> "flag"
