{-# LANGUAGE OverloadedStrings #-}
module System.Terminal.Colors
    ( Color(..)
    , color
    , render
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Read (readMaybe)

import qualified Data.Text as T

data Terminal
    = Dumb
    | Supported

data Color
    = None
    | Black
    | Red
    | Green
    | BrownOrange
    | Blue
    | Purple
    | Cyan
    | LightGray
    | DarkGray
    | LightRed
    | LightGreen
    | Yellow
    | LightBlue
    | LightPurple
    | LightCyan
    | White
    deriving (Show, Read)

escapeCode :: Color -> Text
escapeCode None = "0"
escapeCode Black = "0;30"
escapeCode Red = "0;31"
escapeCode Green = "0;32"
escapeCode BrownOrange = "0;33"
escapeCode Blue = "0;34"
escapeCode Purple = "0;35"
escapeCode Cyan = "0;36"
escapeCode LightGray = "0;37"
escapeCode DarkGray = "1;30"
escapeCode LightRed = "1;31"
escapeCode LightGreen = "1;32"
escapeCode Yellow = "1;33"
escapeCode LightBlue = "1;34"
escapeCode LightPurple = "1;35"
escapeCode LightCyan = "1;36"
escapeCode White = "1;37"

escape :: Color -> Text
escape c = "\ESC[" <> escapeCode c <> "m"

variable :: Color -> Text
variable c = "${" <> T.pack (show c) <> "}$"

color :: Color -> Text -> Text
color c t = variable c <> t <> variable None

render :: Terminal -> Text -> Text
render term t =
    let (l, rest) = T.breakOn "${" t
        (v, r) = T.breakOn "}$" rest
        v' = T.drop 2 v
        r' = T.drop 2 r

    in mconcat [l, replace v', render term r']
  where
    replace v = case (term, readMaybe $ T.unpack v) of
        (Supported, Just c) -> escape c
        (Dumb, Just _) -> ""
        (_, Nothing) -> "${" <> v <> "}$"
