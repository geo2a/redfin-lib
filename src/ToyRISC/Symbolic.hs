{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Symbolic
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- A simple symbolic execution back-end
-- inspired by [SBV](http://hackage.haskell.org/package/sbv)
-- but more fine-tuned for executing low-level code
--
-----------------------------------------------------------------------------
module ToyRISC.Symbolic
    ( Env(..), Engine (..), run
    , State (..), bindings
    , pathCond, model -- lenses for State
    , CVal  (..)
    , Kind  (..)
    , Op    (..)
    , SExpr (..)
    -- concrete values smart constructors
    , sConst, constBool, sTrue, sFalse
    -- symbolic operations smart constructors
    , sOp, sAdd
    -- Aux functions
    , smtType, falseCVal, trueCVal
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Selective
import           Data.Int               (Int32)
import           Data.IORef
import qualified Data.Map               as Map
import           Data.Text
import           Lens.Micro.Platform

import           ToyRISC.Types

-----------------------------------------------------------------------------
-- | Kind of symbolic value
--   Restricted to booleans, 32-bit signed integers and chars,
--   i.e. unsigned 8-bit words
data Kind = KBool
          | KBounded
          | KChar
          deriving (Eq, Ord)

instance Show Kind where
  show KBool    = "SBool"
  show KBounded = "SInt"
  show KChar    = "SChar"

-- | How the kind maps to SMT land
smtType :: Kind -> String
smtType KBool    = "Bool"
smtType KBounded = "(_ BitVec 32)"
smtType KChar    = "(_ BitVec 8)"
-----------------------------------------------------------------------------
-- | Constant value
data CVal = CBounded !Int32
          -- ^ signed 32-bit integer constant
          | CChar !Char
          -- ^ unsigned 8-bit word constant
          | CBool !Bool
          -- ^ boolean constant
          deriving (Eq, Ord)

instance Show CVal where
  show (CBounded i) = show i
  show (CChar c)    = show c
  show (CBool b)    = show b

-- | Constant False as a 'CVal'. We represent it using the integer value 0.
falseCVal :: CVal
falseCVal = CBounded 0

-- | Constant True as a 'CVal'. We represent it using the integer value 1.
trueCVal :: CVal
trueCVal = CBounded 1
-----------------------------------------------------------------------------
data Op = Plus
        | Times
        | Minus
        | Neg -- ^ negate a signed integer
        | Abs

        | Not -- ^ boolean negation
        | And
        | Or

        | Eq
        | Lt
        | Gt
        deriving (Eq, Ord)

instance Show Op where
  show Plus  = "+"
  show Times = "*"
  show Minus = "-"
  show Neg   = "-"
  show Abs   = "abs"
  show Not   = "not"
  show And   = "&&"
  show Or    = "||"
  show Eq    = "=="
  show Lt    = "<"
  show Gt    = ">"
-----------------------------------------------------------------------------
-- | Symbolic expression (not s-expression!!) is either a concrete value or
--   a named variable, or an application of an operation
--   to a list of arguments (symbolic values)
data SExpr = Concrete !CVal
           | Any !Text
           | Symbolic !Op ![SExpr]

instance Show SExpr where
  show (Concrete c)       = show c
  show (Any name)         = unpack name
  show (Symbolic op args) = '(': show op <> " " <> show args <> " "

instance Eq SExpr where
  (Concrete c1) == (Concrete c2) = c1 == c2
  (Any name1)   == (Any name2)   = name1 == name2
  _ == _  =
    error "SExpr.Eq: can't compare symbolic values for now!"

deriving instance Ord SExpr

-- | Form an expression containing an integer constant
sConst :: Int32 -> SExpr
sConst i = Concrete (CBounded i)

-- | Form an expression containing a boolean constant
constBool :: Bool -> SExpr
constBool b = Concrete (CBool b)

sTrue :: SExpr
sTrue = constBool True

sFalse :: SExpr
sFalse = constBool False

sOp :: Op -> [SExpr] -> SExpr
sOp = Symbolic

sAdd :: SExpr -> SExpr -> SExpr
sAdd x y = Symbolic Plus [x, y]

-----------------------------------------------------------------------------
-- | State of the symbolic interpreter
data State modelTy = State
              { _pathCond :: SExpr
              -- ^ path condition: a list of symbolic expressions of kind Bool
              , _model    :: modelTy
              -- ^ the state of the runtime being modelled, i.e. processor or
              --   virtual machine
              } deriving (Show, Eq)

makeLenses ''State

data Env = MkEnv { _bindings :: IORef (Map.Map Key SExpr) }

makeLenses ''Env

-- | It looks like the Selective combinators has mush have nothing to do with
--   symbolic execution per se, i.e. they must just execute both branches somehow,
--   like Over, and somehow forward the symbolic execution to the `read/write` callbacks.
--   Another thing: maybe read :: Bool needs to push into a queue and the path constraint
--   and it's negation and the two branches then will pop it and add to their list of path
--   conditions.

newtype Engine a = MkEngine {getEngine :: Env -> IO a }

instance Functor Engine where
  fmap f (MkEngine x) = MkEngine $ \e ->
    fmap f (x e)

instance Applicative Engine where
  pure = return
  (<*>) = ap

instance Monad Engine where
  x >>= f = MkEngine $ \e -> do
    xRes <- (getEngine x) e
    getEngine (f xRes) e

run :: Engine a -> Env -> IO a
run comp = getEngine comp

instance Selective Engine where
  select = selectA

-- biselect :: Engine (Either a b) -> Engine (Either a c) -> Engine (Either a (b, c))
-- biselect l r = Engine $ \e -> do

solveBool :: SExpr -> IO (Maybe Bool)
solveBool expr = do
  pure (Just False)

whenSEngine :: Engine SExpr -> Engine () -> Engine ()
whenSEngine cond comp = MkEngine $ \env -> do
  condition <- solveBool <$> run cond env

  pure ()

-- branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
-- branch x l r = fmap (fmap Left) x <*? fmap (fmap Right) l <*? r

-- ifS :: Selective f => f Bool -> f a -> f a -> f a
-- ifS x t e = branch (bool (Right ()) (Left ()) <$> x) (const <$> t) (const <$> e)

-- whenS :: Selective f => f Bool -> f () -> f ()
-- whenS x y = select (bool (Right ()) (Left ()) <$> x) (const <$> y)

-- readIO :: Key -> Engine SExpr
-- readIO key = do
--   boundKeys <- liftIO . readIORef . _bindings =<< ask
--   case Map.lookup key boundKeys of
--        Nothing  -> error $ "Undefined key: " <> show key
--        Just val -> do
--          liftIO $ putStrLn $ "Read: " <> show key
--          pure val

-- -- solveBool :: SExpr ->

-- readFlag :: Key -> Engine (SExpr, Maybe Bool)
-- readFlag = \case
--   F flag -> do
--     boundKeys <- liftIO . readIORef . _bindings =<< ask
--     case Map.lookup (F flag) boundKeys of
--          Nothing  -> error $ "Undefined key: " <> show flag
--          Just val ->
--            pure (val, Just False)
--   _ -> error "readFlag: Not a flag key"

-- -- -- writeIO :: Key -> Engine SExpr -> Engine SExpr
-- -- -- writeIO key producer = do
-- -- --   env <- ask
-- -- --   value <- liftIO (runReaderT producer env)
-- -- --   env <- ask
-- -- --   boundKeys <- liftIO . readIORef . bindings $ env
-- -- --   liftIO $ writeIORef (bindings env) (Map.adjust (const value) key boundKeys)
-- -- --   liftIO $ putStrLn $ "Write: (" <> show key <> ", " <> show value <> ")"
-- -- --   pure value

-- -- -- ex :: IO ()
-- -- -- ex = do

-- -- --   print "bye"

-- -- -- -- | A generic symbolic execution mechanism is parameterised by three things:
-- -- --   * a function @pickNext@ for choosing the next state in the worklist
-- -- --   * a function @follow@ that returns a decision whether to follow a branch
-- -- --     or to prune it, i.e. performs a feasibility check
-- -- --   * a relation @~@ that controls whether states should be merged



-- -- -----------------------------------------------------------------------------
