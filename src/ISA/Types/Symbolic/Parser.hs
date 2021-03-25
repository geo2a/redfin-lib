-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.Parser
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Parse symbolic expressions
--
-----------------------------------------------------------------------------
module ISA.Types.Symbolic.Parser (parseSym, pSym, pSAny, pAddress) where

import           Control.Monad.Combinators.Expr
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           ISA.Types
import           ISA.Types.Parser
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address

parseSym :: String -> Text -> Either Text Sym
parseSym symName = either (Left . Text.pack . errorBundlePretty) (Right . id)
         . parse pSym symName

-- pPredicate :: Parser (Sym -> Sym)
-- pPredicate = do
--   (SAny var) <- char '\\' *> pSAny
--   symbol "->"
--   property <- pSym
--   pure (\expr -> subst expr var property)

pCAddress :: Parser CAddress
pCAddress = CAddress <$> lexeme L.decimal <?> "concrete memory address"

pAddress :: Parser Address
pAddress = MkAddress <$>
  choice [ Left  <$> pCAddress
         , Right <$> pSym ]

pInt32 :: Parser Concrete
pInt32 = CInt32 <$>
  L.signed sc (lexeme L.decimal) <?> "Int32 literal"

pWord16 :: Parser Concrete
pWord16 = CWord <$>
  lexeme L.decimal <?> "Word 16 literal"

pBool :: Parser Concrete
pBool = CBool <$>
  lexeme ((string "true" *> pure True) <|>
          (string "false" *> pure False))
  <?> "boolean literal"

pConcrete :: Parser Concrete
pConcrete = pInt32 <|> pWord16 <|> pBool

pSConst :: Parser Sym
pSConst = SConst <$> pConcrete

pSAny :: Parser Sym
pSAny = do
  _ <- char '$'
  name <- lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "variable")
  pure (SAny . Text.pack $ name)

pTerm :: Parser Sym
pTerm = choice
  [ parens pSym
  , pSAny
  , pSConst
  ]

pSym :: Parser Sym
pSym = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Sym]]
operatorTable =
  [ [ prefix "&" SPointer
    , prefix "not" SNot
    , prefix "abs" SAbs]
  , [ binary "&&" SAnd
    , binary "||" SOr
    ]
  , [ binary "*" SMul
    , binary "/" SDiv
    , binary "%" SMod
    ]
  , [ binary "+" SAdd
    , binary "-" SSub
    ]
  , [ binary "==" SEq
    , binary "<" SLt
    , binary ">" SGt
    ]
  ]

binary :: Text -> (Sym -> Sym -> Sym) -> Operator Parser Sym
binary  name f = InfixL  (f <$ symbol name)

prefix :: Text -> (Sym -> Sym) -> Operator Parser Sym
prefix name f = Prefix  (f <$ symbol name)
