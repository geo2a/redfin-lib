{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.TreeTrace
-- Copyright  : (c) Georgy Lucknow 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Pure tree trace-based symbolic execution backend
-----------------------------------------------------------------------------
module ISA.Backend.ListTrace
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
import qualified Data.Map.Strict           as Map
import           GHC.Exts                  (Any)
import           Lens.Micro.Platform
import           Prelude                   hiding (log, not, read, readIO)

import           FS
import           ISA.Semantics
import           ISA.Symbolic
import           ISA.Types

data Context = MkContext { _bindings      :: Map.Map Key Sym
                         , _pathCondition :: Sym
                         , _fmapLog       :: [Any]
                         }

makeLenses ''Context

instance Show Context where
  -- show = Text.unpack . pShow
  show ctx = unlines [ "Path constraint: " <> show (_pathCondition ctx)
                     , "Context: " <> show (Map.toList $ _bindings ctx)
                     -- , "last fmap argument: " <> show (_lastFmap ctx)
                     ]

-- | The Symbolic Execution Engine maintains the state of the machine and a list
--   of path constraints.
data Engine a = Engine
    { runEngine :: Context -> [(a, Context)] }

pushFmapArg :: a -> Engine ()
pushFmapArg res = do
  -- Engine $ \s ->
  -- pure ((), s {_fmapLog = (unsafeCoerce res :: Any) : _fmapLog s})
  s <- get
  put $ s {_fmapLog = (unsafeCoerce res :: Any) : _fmapLog s}

popFmapArg :: Engine a
popFmapArg = do
  -- Engine $ \s ->
  -- let (res:rest) = _fmapLog s
  -- in pure (unsafeCoerce res, s {_fmapLog = rest})
  s <- get
  case (_fmapLog s) of
    [] -> error "Engine.popFmapArg: empty _fmapLog!"
    (res:rest) -> do
      put $ s {_fmapLog = rest}
      pure (unsafeCoerce res)

instance Functor Engine where
  fmap f e =
    Engine $ \s0 -> do
      (res, s) <- runEngine e s0
      ((), s') <- runEngine (pushFmapArg res) s
      pure (f res, s')

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative Engine where
    pure  = return
    (<*>) = ap
    (*>)  = (>>)

instance Selective Engine where
  select cond f = Engine $ \s0 -> do
    -- perform the effectful computation passed as the first argument of select, i.e.
    -- the condition test in @whenS@/@ifS@
    (x, sAfterCond) <- runEngine cond s0
    -- When we call selective combinators with symbolic values we always give them
    -- something that will trigger the @Left@ branch in @select@. In case @x@ contains
    -- a @Right@, we, for now, throw en exception.
    case x of
      Left a -> do
        -- __BLACK MAGIC HERE__
        -- We assume that @cond@ will have form @toBool <$> symbolicExpression@.
        -- If @cond@ comes from @whenS@/@ifS@, it will be then @fmap@'ed one more
        -- time with @bool (Right ()) (Left ())@ to translate @Bool@ into @Either () ()@.
        -- Now, we need to analyse
        -- the @symbolicExpression@ in order to decide whether to execute @f@ or not.
        -- The @Functor@ instance for @Engine@ logs every value that is being @fmap@'ed
        -- over, thus we extract the SECOND TO LAST one, ASSUMING THAT'S THE ONE WE NEED,
        -- and unsafely coerce it to an equality check, i.e. a value of @Equality Sym@.
        -- __NOTE:__ the assumption only holds for @whenS@. Since @ifS@ is implemented
        -- via @branch@ and has more @fmap@s.
        (t, s) <- runEngine (popFmapArg >> popFmapArg) sAfterCond
        case (unsafeCoerce t) of
          -- in case the equality check is trivial, e.g. contains a boolean value,
          -- we can decide whether to execute @f@ or not based on this boolean
          Trivial b      ->
            if b
              then runEngine (($ a) <$> f) s
              else []
          -- Otherwise, there is a non-trivial symbolic equality and we need to
          -- create two possible worlds:
          Nontrivial symbolic ->
            -- the one where the path condition as conjoined with the @symbolic@ itself
            let onTrue  = s { _pathCondition = SAnd symbolic (_pathCondition s) }
            -- and the one where the path constraint is conjoined with the check's negation
                onFalse = s { _pathCondition = SAnd (SNot symbolic) (_pathCondition s) }
            in -- the resulting computation is now a concatenation of two lists:
               -- the list where @symbolic@ holds and we execute @f@
               (runEngine (($ a) <$> f) onTrue) ++
               -- and the list where the negation @symbolic@ holds and we do not execute @f@
               [((unsafeCoerce symbolic) :: b, onFalse)]
      Right _ -> error $
        "Engine.select: Broken assumption! first argument of select returned Right. " <>
        "Check that all occurrences of whenS receive True in case of symbolic code."

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
  pure (MkData x)

writeKey :: Key -> Engine (Data Sym) -> Engine (Data Sym)
writeKey key computation = do
  ctx <- get
  (MkData value) <- computation
  put $ ctx {_bindings = Map.insert key value (_bindings ctx)}
  pure (MkData value)
-----------------------------------------------------------------------------
add' :: Register -> Address -> FS Key Selective Value a
add' reg addr read write =
  let arg1 = read (Reg reg)
      arg2 = read (Addr addr)
      result = (+) <$> arg1 <*> arg2
  in ifS (toBool <$> ((===) <$> write (Reg reg) result <*> pure (fromInteger 0)))
         (write (F Condition) (pure true))
         (write (F Condition) (pure (not true)))

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
                      , _fmapLog = []
                      }
  let t = add R0 0 readKey writeKey
  let xs = runEngine t ctx
  print $ xs
  pure ()
