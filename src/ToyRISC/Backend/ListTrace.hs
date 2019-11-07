{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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

import           Data.Typeable
import           GHC.Exts                  (Any)
import           System.IO.Unsafe          (unsafePerformIO)
import           Unsafe.Coerce
-- import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Selective
import           Data.Either               (either)
import           Data.Int                  (Int32)
import           Data.IORef
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust)
import           Data.Word                 (Word8)
import           Debug.Trace
import           Lens.Micro.Platform
import           Prelude                   hiding (log, not, read, readIO)

import qualified Data.Tree                 as Tree
import           FS
import           ToyRISC.Semantics
import           ToyRISC.SMT
import           ToyRISC.Symbolic
import           ToyRISC.Types

data Context = MkContext { _bindings      :: Map.Map Key Sym
                         , _pathCondition :: Sym
                         , _lastFmap      :: Sym
                         , _inSelect      :: Bool
                         }

makeLenses ''Context

instance Show Context where
  show ctx = unlines [ "Path constraint: " <> show (_pathCondition ctx)
                     , "Context: " <> show (Map.toList $ _bindings ctx)
                     , "last fmap argument: " <> show (_lastFmap ctx)
                     ]

-- | The Symbolic Execution Engine maintains the state of the machine and a list
--   of path constraints.
data Engine a = Engine
    { runEngine :: Context -> [(a, Context)] }
    -- deriving Functor
instance Functor Engine where
  fmap f (Engine e) =
    Engine $ \s0 -> do
      (res, s) <- e s0
      let a = (unsafeCoerce a :: Any)

      -- let z = (cast res :: Maybe (Data (Equality Sym)))
      -- pure (f res, s)
      if (_inSelect s0) then do
        let (MkData z) = (unsafeCoerce res :: Data (Equality Sym))
        -- pure (f res, s')
        case z of
          Solvable b     -> pure ((f res), s {_lastFmap = SConst (CBool b)})
          Unsolvable sym -> pure ((f res), s {_lastFmap = sym})
      else pure (f res, s)
    -- zip (map (f . fst) (e s)) (repeat s)

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative Engine where
    pure  = return
    (<*>) = ap

instance Selective Engine where
  select cond f = Engine $ \s0 -> do
    (x, s) <- runEngine cond (s0 {_inSelect = True})
    let lr = _lastFmap s -- SEq (SAny "x") (SAny ("y")) -- _lastFmap s'
    let opt = solve lr 1000
    case x of
      Left  a ->
        case opt of
          DeadEnd -> error "Engine.select: unsatisfiable path constraint"
          Literal b -> -- [runEngine (when b (f <*> pure b) *> pure b) s']
            error $ "Engine.select: literal path constraint" <> show b
          Sat expr ->
            let onTrue  = s { _pathCondition = SAnd expr (_pathCondition s)
                            , _inSelect = False }
                onFalse = s { _pathCondition = SAnd (SNot expr) (_pathCondition s)
                            , _inSelect = False }
            in -- runEngine (($ a) <$> f) onTrue
               concat [ runEngine (f <*> pure a) onTrue
                      , [((unsafeCoerce lr) :: b, onFalse)]
                      ]
      Right b ->
        error "Engine.select: OMG it's Right! Right is not right when you only use whenS."

-- instance Selective Engine where
--   select cond f = Engine $ \s -> do
--     (x, s') <- runEngine cond s
--     let lr = snd $ _lastRead s'
--     let opt = solveBoolExpr lr
--     case x of
--       Left  a ->
--         case opt of
--           DeadEnd -> error "Engine.select: unsatisfiable path constraint"
--           Literal b -> error $ "Engine.select: literal path constraint" <> show b
--           Sat expr ->
--             let onTrue  = s {_pathCondition = sOp And [expr, _pathCondition s]}
--                 onFalse = s {_pathCondition = sOp And [sOp Not [expr], _pathCondition s]}
--             in -- runEngine (($ a) <$> f) onTrue
--                concat [ runEngine (f <*> pure a) onTrue
--                       , [((unsafeCoerce lr) :: b, onFalse)]
--                       ]
--       Right b ->
--         error "Engine.select: OMG it's Right! Right is not right when you only use whenS."

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

readKey :: Key -> Engine (Data Sym)
readKey key = do
  ctx <- get
  let x = (Map.!) (_bindings ctx) key
  -- put $ ctx {_lastRead = (key, x)}
  pure (MkData x)

writeKey :: Key -> Engine (Data Sym) -> Engine (Data Sym)
writeKey key computation = do
  ctx <- get
  (MkData value) <- computation
  put $ ctx {_bindings = Map.insert key value (_bindings ctx)}
  pure (MkData value)

test :: Engine ()
test =
  let cond = readKey (F Condition)
  in whenS (const True <$> cond) (void $ writeKey (Reg R0) (pure . MkData $ SConst 1))

whenS'' :: (Selective f, Monoid a) => f (Either (Equality a) a) -> f a -> f a
whenS'' x y = x <*? effect
  where
    -- selector :: f (Either Sym a)
    -- selector = analyseSym <$> x
    -- bool (Right mempty) (Left ()) <$> x -- NB: maps True to Left ()
    effect   = const                     <$> y

    analyseSym :: Monoid a => Sym -> Either Sym a
    analyseSym = Left
      -- \case
      -- x -> Left x
      -- _ -> Right mempty

add' :: Register -> Address -> FS Key Selective Value a
add' reg addr read write =
  let arg1 = read (Reg reg)
      arg2 = read (Addr addr)
      result = (+) <$> arg1 <*> arg2
  in whenS'' (Left <$> ((===) <$> write (Reg reg) result <*> pure (fromInteger 0)))
             (write (F Condition) (pure true))

ex :: IO ()
ex = do
  let ctx = MkContext { _pathCondition = SConst (CBool True)
                      , _bindings = Map.fromList [ (IC, SConst 0)
                                                 , (F Condition, SEq (SAny "z") (SConst 0))
                                                 , (Reg R0, SAny "r0")
                                                 , (Reg R1, SAny "r1")
                                                 , (Addr 0, SAny "a0")
                                                 , (Addr 1, SAny "a1")
                                                 ]
                      , _lastFmap = (SEq (SConst 1) (SAny "x"))
                      , _inSelect = False
                      }
  let t = add' R0 0 readKey writeKey *> add' R1 1 readKey writeKey
  -- let t = jumpCt (MkData $ SConst 3) readKey writeKey
  let xs = runEngine t ctx
  print $ xs
  pure ()
