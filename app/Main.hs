{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import OG.Check
import OG.Checks

import Data.Monoid ((<>))
import Data.Text (Text)
import System.Terminal.Colors

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = mapM_ (T.putStr . formatOutcome "") =<< checks

formatOutcome :: Text -> CheckOutcome a -> Text
formatOutcome indent CheckOutcome{..} =
    checkLabel coDetail <>
    formatResult indent coResult <>
    mconcat (map (formatOutcome (indent <> "  ")) coDependents)

formatResult :: Text -> CheckResult a -> Text
formatResult _ (Success _) = ": " <> color Red "OK" <> "\n"
formatResult indent Skipped = ": " <> color Yellow "Skipped" <> "\n"
formatResult indent (Failure msg) = ": " <> color Red "Failure" <> "\n"
    <> indent <> "  " <> msg <> "\n"
formatResult indent (Error err) = ": " <> color Red "ERROR" <> "\n"
    <> indent <> "  " <> err <> "\n"
