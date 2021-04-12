-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
 Module     : ISA.Assembly.Parser
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Parser for the REDFIN assembly language
-}
module ISA.Assembly.Parser () where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
