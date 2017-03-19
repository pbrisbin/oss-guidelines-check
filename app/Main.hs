{-# LANGUAGE OverloadedStrings #-}
module Main where

import OG.Checks

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = forM_ checks $ \check -> do
    result <- checkResult check
    T.putStr $ formatCheckResult "" (checkDetails check, result)

formatCheckResult :: Text -> (CheckDetails, CheckResult a) -> Text
formatCheckResult indent (cd, Success _) = indent <> simple cd "OK"
formatCheckResult indent (cd, Skipped) = indent <> simple cd "Skipped"
formatCheckResult indent (cd, Failure msg) =
    indent <> simple cd "Failure" <>
    indent <> "  " <> T.pack msg <> "\n"
formatCheckResult indent (cd, Error err) =
    indent <> simple cd "Error" <>
    indent <> "  " <> T.pack err <> "\n"
formatCheckResult indent (_, (Multi (check:checks))) =
    formatCheckResult indent check <>
    mconcat (map (formatCheckResult (indent <> "  ")) checks)

-- Should be impossible
formatCheckResult indent (_, (Multi [])) = ""

simple :: CheckDetails -> Text -> Text
simple (CheckDetails n) label = T.pack n <> ": " <> label <> "\n"
