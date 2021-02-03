{-# LANGUAGE OverloadedStrings #-}

module ISA.Types.Symbolic.Parser (parseSym, parseProp) where

import           Control.Monad.Combinators.Expr
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer        as L

import           ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Trace.Property

parseSym :: String -> Text -> Either Text Sym
parseSym symName = either (Left . Text.pack . errorBundlePretty) (Right . id)
         . parse pSym symName

parseProp :: String -> Text -> Either Text Property
parseProp propName =
  either (Left . Text.pack . errorBundlePretty) (Right . id)
  . parse pProp propName

pProp :: Parser Property
pProp = symbol "whole" *>
        (InWhole <$> pValidity <*> pKey <*> pPredicate)
        <|>
        symbol "leafs" *>
        (InLeafs <$> pValidity <*> pKey <*> pPredicate)

pValidity :: Parser Validity
pValidity = (symbol "allsat" *> pure AllSat)
        <|> (symbol "allunsat" *> pure AllUnsat)

pPredicate :: Parser (Sym -> Sym)
pPredicate = do
  (SAny var) <- char '\\' *> pSAny
  symbol "->"
  property <- pSym
  pure (\expr -> subst expr var property)

pKey :: Parser Key
pKey = Reg <$> pReg
   <|> F   <$> pFlag

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

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

-- | Skip spaces
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pInt32 :: Parser Concrete
pInt32 = CInt32 <$>
  L.signed sc (lexeme L.decimal) <?> "Int32 literal"

pWord16 :: Parser Concrete
pWord16 = CWord <$>
  lexeme L.decimal <?> "Word 16 literal"

pBool :: Parser Concrete
pBool = CBool <$>
  lexeme ((string "true" *> pure True) <|> (string "false" *> pure False)) <?> "boolean literal"

pConcrete :: Parser Concrete
pConcrete = pInt32 <|> pWord16 <|> pBool

pSConst :: Parser Sym
pSConst = SConst <$> pConcrete

pSAny :: Parser Sym
pSAny = SAny . Text.pack <$> lexeme
  ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "variable")

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
  [ [ prefix "not" SNot
    , prefix "abs" SAbs]
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
  , [ binary "&&" SAnd
    , binary "||" SOr
    ]
  ]

binary :: Text -> (Sym -> Sym -> Sym) -> Operator Parser Sym
binary  name f = InfixL  (f <$ symbol name)

prefix :: Text -> (Sym -> Sym) -> Operator Parser Sym
prefix name f = Prefix  (f <$ symbol name)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
