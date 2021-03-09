-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Parser
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- A parser infrastructure via Megaparsec
--
-----------------------------------------------------------------------------

module ISA.Types.Parser
    (Parser, sc, lexeme, symbol) where

import           Data.Text                  (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

-- | Skip spaces
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
