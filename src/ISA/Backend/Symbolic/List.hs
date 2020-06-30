
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module     : ISA.Backend.Symbolic.List
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Pure tree trace-based symbolic execution backend
-----------------------------------------------------------------------------
module ISA.Backend.Symbolic.List
    ( -- * symbolic execution state
      Context(..), showKey
      -- * symbolic execution engine
    , Engine (..)
      -- instances of read/write callbacks for Engine
    , readKey, writeKey
    ) where

import           Control.Monad.Reader
import           Control.Monad.State          (evalState)
import           Control.Monad.State.Class
-- import           Control.Selective
import qualified Data.Map.Strict              as Map
import           GHC.Exts                     (Any)
import           Prelude                      hiding (log, not, read, readIO)
import           Unsafe.Coerce

import           ISA.Selective
import           ISA.Semantics
import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Trace

-- | A record type for state of the symbolically executed computation
--   * @_bindings@: keys (like register names, memory cells) mapped to their symbolic values
--   * @_pathCondition@ : a symbolic expression which must hold for this state to be
--     reachable
--   * @_fmapLog@: a stack of values of @fmap@'s second arguments.
--     Consider @fmap (f :: a -> b) (x :: Engine a)@, then after executing this
--     computations: @head (_fmapLog s)@ will contain the "purified" @x@, i.e.
--     a value of type @a@ coerced to 'GHC.Exts.Any'. This field is modified
--     via 'pushFmapArg' and 'popFmapArg' functions.
--     (TODO: come up with a better explanation for this)
data Context = MkContext { _bindings      :: Map.Map Key Sym
                         , _pathCondition :: Sym
                         , _fmapLog       :: [Any]
                         }

showKey :: Context -> Key -> String
showKey ctx key =
  case Map.lookup key (_bindings ctx) of
    Nothing -> "uninitialised"
    Just v  ->
      if key == IR
      then show key <> ": " <> show (toInstruction v)
      else show key <> ": " <> show v

instance Show Context where
  show ctx = unlines [ "Path constraint: " <> show (_pathCondition ctx)
                     , showKey ctx IR
                     , showKey ctx (F Condition)
                     , showKey ctx (F Halted)
                     ]

-- | A Symbolic Execution Engine is a combination of State and List monads:
--   given a state, it produces a list of new possible states. Imagine conditional
--   branch: one state (before branch) produces two new states, one where the condition
--   is true and one where it's false.
--
--   A fun note: isn't this a monadic parser? How can we explore a connection between
--   symbolic execution and parsing and turn it into a functional pearl for ICFP...
data Engine a = Engine
    { runEngine :: Context -> [(a, Context)] }

{-# WARNING pushFmapArg "This function uses @unsafeCoerce@." #-}
pushFmapArg :: a -> Engine ()
pushFmapArg res = do
  s <- get
  put $ s {_fmapLog = (unsafeCoerce res :: Any) : _fmapLog s}

{-# WARNING popFmapArg "This function uses @unsafeCoerce@." #-}
popFmapArg :: Engine a
popFmapArg = do
  s <- get
  case (_fmapLog s) of
    [] -> error "Engine.popFmapArg: empty _fmapLog!"
    (res:rest) -> do
      put $ s {_fmapLog = rest}
      pure (unsafeCoerce res)

-- | The 'Functor' instance for @Engine@, besides fmapping the result of the
--   computations performs additional work: it logs the "purified" value of @fmap@'s
--   second argument via 'pushFmapArg'.
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
  select condition boolElim dataElim = Engine $ \s0 -> do
    (cond, s) <- runEngine condition s0
    case (trySolve . unsafeCoerce $ cond) of
      Trivial True  -> runEngine (boolElim True) s
      Trivial False -> runEngine (boolElim False) s
      Nontrivial symbolic ->
        let onTrue  = s { _pathCondition = SAnd symbolic (_pathCondition s) }
            onFalse = s { _pathCondition = SAnd (SNot symbolic) (_pathCondition s) }
        in (runEngine (($ (unsafeCoerce symbolic)) <$> dataElim) onTrue) ++
           [((unsafeCoerce symbolic) :: b, onFalse)]

-- instance Selective Engine where
--   select cond f = Engine $ \s0 -> do
--     (x, sAfterCond) <- runEngine cond s0
--     case x of
--       Left a -> do
--         (t, s) <- runEngine (popFmapArg >> popFmapArg) sAfterCond
--         case (trySolve . unsafeCoerce $ t) of
--           Trivial b -> if b then runEngine (($ a) <$> f) s
--                             else []
--           Nontrivial symbolic ->
--             -- the one where the path condition as conjoined with the @symbolic@ itself
--             let onTrue  = s { _pathCondition = SAnd symbolic (_pathCondition s) }
--             -- and the one where the path constraint is conjoined with the check's negation
--                 onFalse = s { _pathCondition = SAnd (SNot symbolic) (_pathCondition s) }
--             in -- the resulting computation is now a concatenation of two lists:
--                -- the list where @symbolic@ holds and we execute @f@
--                (runEngine (($ a) <$> f) onTrue) ++
--                -- and the list where the negation @symbolic@ holds and we do not execute @f@
--                [((unsafeCoerce symbolic) :: b, onFalse)]
--       Right _ -> error $
--         "Engine.select: Broken assumption! first argument of select returned Right. " <>
--         "Check that all occurrences of whenS receive True in case of symbolic code."
  -- select cond f = Engine $ \s0 -> do
  --   -- perform the effectful computation passed as the first argument of select, i.e.
  --   -- the condition test in @whenS@/@ifS@
  --   (x, sAfterCond) <- runEngine cond s0
  --   -- When we call selective combinators with symbolic values we always give them
  --   -- something that will trigger the @Left@ branch in @select@. In case @x@ contains
  --   -- a @Right@, we, for now, throw en exception.
  --   case x of
  --     Left a -> do
  --       -- __BLACK MAGIC HERE__
  --       -- We assume that @cond@ will have form @toBool <$> symbolicExpression@.
  --       -- If @cond@ comes from @whenS@/@ifS@, it will be then @fmap@'ed one more
  --       -- time with @bool (Right ()) (Left ())@ to translate @Bool@ into @Either () ()@.
  --       -- Now, we need to analyse
  --       -- the @symbolicExpression@ in order to decide whether to execute @f@ or not.
  --       -- The @Functor@ instance for @Engine@ logs every value that is being @fmap@'ed
  --       -- over, thus we extract the SECOND TO LAST one, ASSUMING THAT'S THE ONE WE NEED,
  --       -- and unsafely coerce it to an equality check, i.e. a value of @Equality Sym@.
  --       -- __NOTE:__ the assumption only holds for @whenS@. Since @ifS@ is implemented
  --       -- via @branch@ and has more @fmap@s.
  --       (t, s) <- runEngine (popFmapArg >> popFmapArg) sAfterCond
  --       case (unsafeCoerce t) of
  --         -- in case the equality check is trivial, e.g. contains a boolean value,
  --         -- we can decide whether to execute @f@ or not based on this boolean
  --         Trivial b      ->
  --           if b
  --             then runEngine (($ a) <$> f) s
  --             else []
  --         -- Otherwise, there is a non-trivial symbolic equality and we need to
  --         -- create two possible worlds:
  --         Nontrivial symbolic ->
  --           -- the one where the path condition as conjoined with the @symbolic@ itself
  --           let onTrue  = s { _pathCondition = SAnd symbolic (_pathCondition s) }
  --           -- and the one where the path constraint is conjoined with the check's negation
  --               onFalse = s { _pathCondition = SAnd (SNot symbolic) (_pathCondition s) }
  --           in -- the resulting computation is now a concatenation of two lists:
  --              -- the list where @symbolic@ holds and we execute @f@
  --              (runEngine (($ a) <$> f) onTrue) ++
  --              -- and the list where the negation @symbolic@ holds and we do not execute @f@
  --              [((unsafeCoerce symbolic) :: b, onFalse)]
  --     Right _ -> error $
  --       "Engine.select: Broken assumption! first argument of select returned Right. " <>
  --       "Check that all occurrences of whenS receive True in case of symbolic code."

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
    let x = case Map.lookup key (_bindings ctx) of
              Just v -> v
              Nothing -> error $ "Engine.readKey: uninitialised key " <> show key
    pure (MkData x)

writeKey :: Key -> Engine (Data Sym) -> Engine (Data Sym)
writeKey key computation = do
  ctx <- get
  (MkData value) <- computation
  put $ ctx {_bindings = Map.insert key value (_bindings ctx)}
  pure (MkData value)
-----------------------------------------------------------------------------
