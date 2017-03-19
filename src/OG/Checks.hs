{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module OG.Checks where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (second)
import Data.Text (Text)
import Data.List (elemIndex)
import System.FilePath.Glob (glob)

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as T

data CheckResult a
    = Success a
    | Failure String
    | Error String
    | Skipped
    | Multi [(CheckDetails, CheckResult a)]

instance Functor CheckResult where
    fmap f (Success a) = Success $ f a
    fmap _ (Failure msg) = Failure msg
    fmap _ (Error err) = Failure err
    fmap _ Skipped = Skipped
    fmap f (Multi subchecks) = Multi $ map (second (f <$>)) subchecks

-- For now, just a Name
newtype CheckDetails = CheckDetails String

data Check m a = Check
    { checkDetails :: CheckDetails
    , checkResult :: m (CheckResult a)
    }

instance Functor m => Functor (Check m) where
    fmap f c = c { checkResult = (f <$>) <$> checkResult c }

checks :: MonadIO m => [Check m ()]
checks =
    [ void $ Check (CheckDetails "LICENSE present") (pathExists "LICENSE")
    , dependent (Check (CheckDetails "README present") (pathExists "README*"))
        [ (CheckDetails "README Usage", (`hasLine` "## Usage"))
        , (CheckDetails "README Development & Test", (`hasLine` "## Development & Test"))
        ]
    , dependent (Check (CheckDetails "Makefile present") (pathExists "Makefile"))
        [ (CheckDetails "Makefile test target", (`hasLine` "test:"))
        ]
    , dependent (Check (CheckDetails "CHANGELOG present") (pathExists "CHANGELOG*"))
        [ (CheckDetails "CHANGELOG Unreleased", (`hasLine` "## [Unreleased]"))
        ]
    ]

dependent :: Monad m => Check m a -> [(CheckDetails, a -> m (CheckResult b))] -> Check m ()
dependent c fs = c
    { checkResult = do
        result <- checkResult c
        dependentResults <- case result of
            Success x -> mapM (\(cd, f) -> (cd,) . void <$> f x) fs
            _ -> return $ map (second (const Skipped)) fs
        return $ Multi $ (checkDetails c, void result) : dependentResults
    }

pathExists :: MonadIO m => String -> m (CheckResult FilePath)
pathExists p = liftIO $ try $ do
    matches <- glob p
    return $ case matches of
        (x:_) -> Success x
        _ -> Failure $ "No files found matching \"" ++ p ++ "\""

hasLine :: MonadIO m => FilePath -> Text -> m (CheckResult Int)
hasLine fp ln = liftIO $ try $ do
    lns <- T.lines <$> T.readFile fp
    maybe (return failure) (return . Success) (ln `elemIndex` lns)

  where
    failure :: CheckResult Int
    failure = Failure $
        "The line \"" ++ T.unpack ln ++ "\" was not found in " ++ fp

hasContent :: MonadIO m => FilePath -> Text -> m (CheckResult ())
hasContent fp txt = liftIO $ try $ do
    content <- T.readFile fp
    return $ if txt `T.isInfixOf` content
        then Success ()
        else Failure $
            "The content \"" ++ T.unpack txt ++ "\" was not found in " ++ fp

try :: IO (CheckResult a) -> IO (CheckResult a)
try f = either err id <$> E.try f

  where
    err :: E.IOException -> CheckResult a
    err = Error . show
