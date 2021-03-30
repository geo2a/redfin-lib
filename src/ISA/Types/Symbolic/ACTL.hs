-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Types.Symbolic.ACTL
-- Copyright  : (c) Georgy Lukyanov 2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- State properties as ACTL formulas --- universal fragment of CTL
--
-----------------------------------------------------------------------------

module ISA.Types.Symbolic.ACTL where

import           Control.Monad.Combinators.Expr
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Prelude                        hiding (not)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           ISA.Types.Context
import           ISA.Types.Key
import           ISA.Types.Parser
import           ISA.Types.Prop
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Parser

-- | (Restricted) Universal fragment of CTL
data ACTL where
  -- | All Globally
  ACTLAllG :: Atom -> ACTL
  -- | All Finally
  ACTLAllF :: Atom -> ACTL  
  -- | Conjunction
  ACTLAnd :: ACTL -> ACTL -> ACTL
  -- -- | Disjunction
  -- ACTLOr :: ACTL -> ACTL -> ACTL
  -- -- | All Next --- must hold in the next state alongside every path
  -- AllX :: ACTL -> ACTL

deriving instance Show ACTL

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
  AOr  :: Atom -> Atom -> Atom
  AAnd :: Atom -> Atom -> Atom
  -- | Equality of atoms, say the value of a key and a symbolic expression
  AEq  :: Atom -> Atom -> Atom
  -- | Greater-than
  AGt  :: Atom -> Atom -> Atom
  -- | Less-than
  ALt  :: Atom -> Atom -> Atom

deriving instance Show Atom

parseAtom :: String -> Text -> Either Text Atom
parseAtom name = either (Left . Text.pack . errorBundlePretty) (Right . id)
         . parse pAtom name

pTerm :: Parser Atom
pTerm = choice
  [ parens pAtom
  , AKey <$> brackets (pKey <?> "key")
  , ASym <$> curly (pSym <?> "symbolic expression")
  ]

pAtom :: Parser Atom
pAtom = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Atom]]
operatorTable =
  [ [ prefix "!" ANot]
  , [ binary ".==" AEq
    , binary ".<" ALt
    , binary ".>" AGt
    ]
  , [ binary "&&&" AAnd
    , binary "|||" AOr
    ]
  ]

binary :: Text -> (Atom -> Atom -> Atom) -> Operator Parser Atom
binary  name f = InfixL (f <$ symbol name)

prefix :: Text -> (Atom -> Atom) -> Operator Parser Atom
prefix name f = Prefix (f <$ symbol name)

-- | Evaluate an atomic formula at the given state
evalAtom :: Atom -> Context Sym -> Sym
evalAtom atom ctx = go true atom
  where
    go acc = \case
      AKey k   -> let x = getBinding k ctx
                  in maybe false id x
      ASym s   -> s
      ANot p   -> acc &&& not (evalAtom p ctx)
      AAnd x y -> acc &&& (evalAtom x ctx &&&  evalAtom y ctx)
      AOr  x y -> acc &&& (evalAtom x ctx |||  evalAtom y ctx)
      AEq  x y -> acc &&& (evalAtom x ctx ===  evalAtom y ctx)
      AGt  x y -> acc &&& (evalAtom x ctx `lt` evalAtom y ctx)
      ALt  x y -> acc &&& (evalAtom x ctx `gt` evalAtom y ctx)

-- -- | Property is what model checking literature calls a _state predicate_
-- --   For example, it may can say that the value of a certain register
-- --   is non-negative etc.
-- newtype Property = Property (Context Sym -> Sym)

-- evalProperty :: Property -> Context Sym -> Sym
-- evalProperty (Property p) ctx = p ctx

-- -- | The 'Num' instance for 'Property' allows using numeric literals when
-- --   formulating properties
-- instance Num Property where
--   (Property x) + (Property y) = Property $ \s -> SAdd (x s) (y s)
--   (Property x) - (Property y) = Property $ \s -> SSub (x s) (y s)
--   (Property x) * (Property y) = Property $ \s -> SMul (x s) (y s)
--   abs(Property  x) = Property $ \s -> SAbs (x s)
--   signum _ = error "Property.Num: signum is not defined"
--   fromInteger x = Property $ const (SConst (CInt32 $ fromInteger x))
--   negate (Property x) = Property $ \s ->
--     case x s of
--       SConst (CInt32 i) -> SConst (CInt32 (negate i))
--       _ -> error "Property.Num: negate is not defined for non-Int32 values"

-- instance Boolean Property where
--   true   = Property $ const true
--   false  = Property $ const false
--   toBool = error "Boolean.Property: not implemented"
--   fromBool b = Property $ const (fromBool b)
--   not (Property p)   = Property $ \s -> not (p s)
--   (Property p) ||| (Property q) = Property $ \s -> p s ||| q s
--   (Property p) &&& (Property q) = Property $ \s -> p s &&& q s

-- instance TryEq Property where
--   (Property p) === (Property q) = Property $ \s -> p s === q s

-- instance TryOrd Property where
--   lt (Property p) (Property q) = Property $ \s -> p s `lt` q s
--   gt (Property p) (Property q) = Property $ \s -> p s `gt` q s

pTheorem :: Parser ACTL
pTheorem = char 'G' *> sc *> (ACTLAllG <$> parens pAtom)
       <|> char 'F' *> sc *> (ACTLAllF <$> parens pAtom)

parseTheorem :: String -> Text -> Either Text ACTL
parseTheorem name =
  either (Left . Text.pack . errorBundlePretty) (Right . id)
  . parse pTheorem name

