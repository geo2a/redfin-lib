{- |
 Module     : ISA.Backend.Symbolic.Zipper.Save
 Copyright  : (c) Georgy Lukyanov 2019
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Save/load symbolic execution traces to disk
-}
module ISA.Backend.Symbolic.Zipper.Save (saveTrace, loadTrace) where

import Control.Concurrent.STM
import Control.Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import ISA.Backend.Symbolic.Zipper

-- | Save the trace into a file
saveTrace :: FilePath -> Trace -> IO (Either Text ())
saveTrace fpath trace =
    try (LB.writeFile fpath . Aeson.encode $ trace) >>= \case
        Left (_ :: SomeException) -> pure (Left "I/O error: target file does not exist?")
        Right _ -> pure (Right ())

-- | Load a trace from file
loadTrace :: FilePath -> IO (Either Text Trace)
loadTrace fpath = do
    x <- try (LB.readFile fpath)
    case x of
        Left (_ :: SomeException) -> pure (Left "I/O error: target file does not exist?")
        Right txt ->
            case Aeson.eitherDecode txt of
                Left err ->
                    pure $ Left (Text.pack err)
                Right trace ->
                    pure (Right trace)
