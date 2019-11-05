{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Backend.TreeTrace
-- Copyright  : (c) Georgy Lucknow 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Pure tree trace-based symbolic execution backend
-----------------------------------------------------------------------------
module ToyRISC.Backend.ListTrace
    -- ( Env(..), Engine (..), run
    -- , State (..), bindings
    -- , pathCond, model -- lenses for State
    -- , ) where
  where

import           Unsafe.Coerce
-- import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Selective
import           Data.Either               (either)
import           Data.IORef
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust)
import           Data.Word                 (Word8)
import           Lens.Micro.Platform
import           Prelude                   hiding (log, read, readIO)

import qualified Data.Tree                 as Tree
import           ToyRISC.SMT
import           ToyRISC.Symbolic
import           ToyRISC.Types

data Context = MkContext { _bindings      :: Map.Map Key SExpr
                         , _pathCondition :: SExpr
                         , _lastRead      :: (Key, SExpr)
                         }

makeLenses ''Context

instance Show Context where
  show ctx = unlines [ show (_pathCondition ctx)
                     , show (Map.toList $ _bindings ctx)
                     ]

type WorldId = Word8

-- | The Symbolic Execution Engine maintains the state of the machine and a list
--   of path constraints.
data Engine a = Engine
    { runEngine :: Context -> [(a, Context)] }
    deriving Functor

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative Engine where
    pure  = return
    (<*>) = ap

instance Selective Engine where
  select cond f = Engine $ \s -> do
    (x, s') <- runEngine cond s
    let lr = snd $ _lastRead s'
    let opt = solveBoolExpr lr
    case x of
      Left  a ->
        case opt of
          DeadEnd -> error "Engine.select: unsatisfiable path constraint"
          Literal b -> error "Engine.select: literal path constraint"
          Sat expr ->
            let onTrue  = s {_pathCondition = sOp And [expr, _pathCondition s]}
                onFalse = s {_pathCondition = sOp And [sOp Not [expr], _pathCondition s]}
            in -- runEngine (($ a) <$> f) onTrue
               concat [ runEngine (f <*> pure a) onTrue
                      , [((unsafeCoerce lr) :: b, onFalse)]
                      ]
      Right b ->
        error "Engine.select: OMG it's Right! Right is not right when you only use whenS."

-- | A 'Monad' instance for 'Engine' is a combination of state and list monads:
--   * 'return' embeds the value paired with the current state into a list
--   * '>>=' exploits the list monad to run 'f' on every result of 'computation' and
--     join the resulting lists of results.
instance Prelude.Monad Engine where
    return a       = Engine $ \s -> [(a, s)]
    Engine computation >>= f = Engine $ \s -> do
      (result, s') <- computation s
      runEngine (f result) s'

instance (MonadState Context) Engine where
    get   = Engine $ \s -> [(s, s)]
    put s = Engine $ \_ -> [((), s)]

readKey :: Key -> Engine SExpr
readKey key = do
  ctx <- get
  let x = (Map.!) (_bindings ctx) key
  put $ ctx {_lastRead = (key, x)}
  pure x

writeKey :: Key -> Engine SExpr -> Engine SExpr
writeKey key computation = do
  ctx <- get
  value <- computation
  put $ ctx {_bindings = Map.insert key value (_bindings ctx)}
  pure value

data Options = DeadEnd
             -- ^ the path constraint is unsatisfiable
             | Literal Bool
             -- ^ the path constraint is a literal boolean value, continue in this world
             | Sat SExpr
             -- ^ the path constraint is a satisfiable symbolic boolean -- need to create
             --   two worlds: for it and its negation

solveBoolExpr :: SExpr -> Options
solveBoolExpr = \case
  Concrete (CBool b)  -> Literal b
  expr@(Any _)        -> Sat expr
  expr@(Symbolic _ _) -> Sat expr

test :: Engine ()
test =
  let cond = readKey (F Condition)
  in whenS (const True <$> cond) (void $ writeKey (Reg R0) (pure $ sConst 1))

ex :: IO ()
ex = do
  let ctx = MkContext { _pathCondition = sTrue
                      , _bindings = Map.fromList [ (IC, sConst 0)
                                                 , (F Condition, Any "z")
                                                 , (Reg R0, sConst $ -1)
                                                 , (Reg R1, sConst $ -1)
                                                 ]
                      , _lastRead = (IC, sConst $ -1)
                      }
  let xs = runEngine test ctx
  print $ xs
