{- |
 Module     : ISA.Types.Symbolic.ACTL
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 ACTL formulas (universal fragment of CTL) for specification of safety
 properties of programs
-}
module ISA.Types.Symbolic.ACTL (
    ACTL (..),
    negateACTL,
    Atom (..),
    parseTheorem,
) where

import Control.Monad.Combinators.Expr
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHC.Generics as GHC
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (not)

import ISA.Types.Key
import ISA.Types.Parser
import ISA.Types.Symbolic
import ISA.Types.Symbolic.Parser

{- | (Restricted) Universal fragment of CTL
   We want to specify /safety/ properties, thus we consider atomic
   state predicates that should hold either globally or at some point
   and then forever in future and conjunctions thereof
-}
data ACTL where
    -- | All Globally, must hold in all states
    ACTLAllG :: Atom -> ACTL
    -- | All Finally, must hold in all paths from some point
    ACTLAllF :: Atom -> ACTL
    -- | Conjunction
    ACTLAnd :: ACTL -> ACTL -> ACTL

deriving instance Show ACTL
deriving instance GHC.Generic ACTL
instance Aeson.ToJSON ACTL where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON ACTL

-- | Negation is only allowed for atomic state predicates
negateACTL :: ACTL -> ACTL
negateACTL = \case
    ACTLAllG atom -> ACTLAllG (ANot atom)
    ACTLAllF atom -> ACTLAllF (ANot atom)
    p@(ACTLAnd _ _) ->
        error $ "Negation allowed only for atomic properties; property: " <> show p

-- | Atomic propositions about a program state
data Atom where
    -- | Assess the value of a specific 'ISA.Types.Key' in the state
    AKey :: Key -> Atom
    -- | Embed a symbolic value into an 'Atom'
    ASym :: Sym -> Atom
    -- | Negation is allowed only on 'Atom's, not on ACTL formulas
    ANot :: Atom -> Atom
    AOr :: Atom -> Atom -> Atom
    AAnd :: Atom -> Atom -> Atom
    -- | Equality of atoms, say the value of a key and a symbolic expression
    AEq :: Atom -> Atom -> Atom
    -- | Greater-than
    AGt :: Atom -> Atom -> Atom
    -- | Less-than
    ALt :: Atom -> Atom -> Atom

deriving instance Show Atom
deriving instance GHC.Generic Atom
instance Aeson.ToJSON Atom where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Atom

--------------------------------------------------------------------------------
pTerm :: Parser Atom
pTerm =
    choice
        [ parens pAtom
        , AKey <$> brackets (pKey <?> "key")
        , ASym <$> curly (pSym <?> "symbolic expression")
        ]

pAtom :: Parser Atom
pAtom = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Atom]]
operatorTable =
    [ [prefix "!" ANot]
    ,
        [ binary ".==" AEq
        , binary ".<" ALt
        , binary ".>" AGt
        ]
    ,
        [ binary "&&&" AAnd
        , binary "|||" AOr
        ]
    ]

binary :: Text -> (Atom -> Atom -> Atom) -> Operator Parser Atom
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (Atom -> Atom) -> Operator Parser Atom
prefix name f = Prefix (f <$ symbol name)

pTheorem :: Parser ACTL
pTheorem =
    char 'G' *> sc *> (ACTLAllG <$> parens pAtom)
        <|> char 'F' *> sc *> (ACTLAllF <$> parens pAtom)

parseTheorem :: String -> Text -> Either Text ACTL
parseTheorem name =
    either (Left . Text.pack . errorBundlePretty) (Right . id)
        . parse pTheorem name
