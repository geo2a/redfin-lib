{-# LANGUAGE DeriveAnyClass #-}

module ISA.Types.Key (
    -- ** Abstraction over possible locations in the ISA
    Key (..),
    parseKey,
    pKey,
    keyTag,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char

import ISA.Types
import ISA.Types.Parser
import ISA.Types.Symbolic.Address
import ISA.Types.Symbolic.Parser

-- | Abstraction over possible locations in the ISA
data Key where
    -- | data register
    Reg :: Register -> Key
    -- | memory cell
    Addr :: Address -> Key
    -- | flag, a special boolean register
    F :: Flag -> Key
    -- | instruction counter
    IC :: Key
    -- | instruction register
    IR :: Key
    -- | program address
    Prog :: Address -> Key

deriving instance Eq Key
deriving instance Ord Key
deriving instance Generic Key

deriving instance Aeson.ToJSON Key
deriving instance Aeson.FromJSON Key

instance Aeson.FromJSONKey Key where
    fromJSONKey = Aeson.FromJSONKeyTextParser $ \t -> case parseKey t of
        Right k -> pure k
        Left err -> fail ("Invalid key: " <> Text.unpack err)
instance Aeson.ToJSONKey Key where
    toJSONKey = Aeson.toJSONKeyText (Text.pack . show)

keyTag :: Key -> String
keyTag = \case
    Reg _ -> "register"
    Addr _ -> "address"
    F _ -> "flag"
    IC -> "instruction-counter"
    IR -> "instruction-register"
    Prog _ -> "program-address"

instance Show Key where
    show = \case
        Reg reg -> show reg
        Addr addr -> show addr
        F flag -> show flag
        IC -> "IC"
        IR -> "IR"
        Prog addr -> "PROG " <> show addr

parseKey :: Text -> Either Text Key
parseKey =
    either (Left . Text.pack . errorBundlePretty) (Right . id)
        . parse (pKey <* eof) ""

pKey :: Parser Key
pKey =
    (Reg <$> pReg)
        <|> (Addr <$> pAddress)
        <|> (F <$> pFlag)
        <|> ((IC <$ symbol "IC" <?> "instruction counter key label"))
        <|> ((IR <$ symbol "IR" <?> "instruction register key label"))
        <|> (Prog <$> (symbol "PROG" *> pAddress <?> "program address key"))
        <?> "key"

pReg :: Parser Register
pReg =
    char 'R'
        *> choice
            [ R0 <$ symbol "0"
            , R1 <$ symbol "1"
            , R2 <$ symbol "2"
            , R3 <$ symbol "3"
            ]
            <?> "register"

pFlag :: Parser Flag
pFlag =
    (Halted <$ symbol "Halted")
        <|> (Overflow <$ symbol "Overflow")
        <|> (Condition <$ symbol "Condition")
        <?> "flag"
