{-# LANGUAGE OverloadedStrings #-}
module OG.Checks.Support where

import OG.Check

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.List (elemIndex)
import Data.Monoid ((<>))
import System.FilePath.Glob (glob)

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as T

pathExists :: MonadIO m => String -> m (CheckResult FilePath)
pathExists p = liftIO $ try $ do
    matches <- glob p
    return $ case matches of
        (x:_) -> Success x
        _ -> Failure $ "No files found matching \"" <> T.pack p <> "\""

hasLine :: MonadIO m => FilePath -> Text -> m (CheckResult Int)
hasLine fp ln = liftIO $ try $ do
    lns <- T.lines <$> T.readFile fp
    maybe (return failure) (return . Success) (ln `elemIndex` lns)

  where
    failure :: CheckResult Int
    failure = Failure $
        "The line \"" <> ln <> "\" was not found in " <> T.pack fp

hasContent :: MonadIO m => FilePath -> Text -> m (CheckResult ())
hasContent fp txt = liftIO $ try $ do
    content <- T.readFile fp
    return $ if txt `T.isInfixOf` content
        then Success ()
        else Failure $
            "The content \"" <> txt <> "\" was not found in " <> T.pack fp

try :: IO (CheckResult a) -> IO (CheckResult a)
try f = either err id <$> E.try f

  where
    err :: E.IOException -> CheckResult a
    err = Error . T.pack . show
