{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Semantics
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Semantics of ToyRISC instructions
-----------------------------------------------------------------------------
module ToyRISC.Semantics
    ( add
    , jumpCt
    , whenS'
    ) where

import           Control.Selective
import           Data.Bool         (bool)
-- import           Data.Functor      (void)
import           Prelude           hiding (Read, read, readIO)

import           FS
import           ToyRISC.Types

-----------------------------------------------------------------------------
-- | A valiant of 'Control.Selective.whenS'
-- whenS' :: (Selective f, Monoid a) => f Bool -> f a -> f a
-- whenS' x y = select (bool (Right mempty) (Left ()) <$> x) (const <$> y)

whenS' :: (Selective f, Monoid a) => f Bool -> f a -> f a
whenS' x y = selector <*? effect
  where
    selector = bool (Right mempty) (Left ()) <$> x -- NB: maps True to Left ()
    effect   = const                     <$> y
-----------------------------------------------------------------------------

add :: Register -> Address -> FS Key Selective Value a
add reg addr read write =
  let arg1 = read (Reg reg)
      arg2 = read (Addr addr)
      result = (+) <$> arg1 <*> arg2
  in whenS' (toBool <$> ((==) <$> write (Reg reg) result <*> pure mempty))
            (write (F Condition) (pure true))


jumpCt :: a -> FS Key Selective Value a
jumpCt offset read write =
    whenS' (toBool <$> read (F Condition))
           (write IC ((+) <$> pure offset
                          <*> read IC))
-----------------------------------------------------------------------------
