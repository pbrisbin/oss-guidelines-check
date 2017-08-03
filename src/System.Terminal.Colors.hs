module System.Terminal.Colors where

import Data.Text (Text)
import Data.String (IsString, fromString)

data Color
    = Green
    | Yellow
    | Red
    | None

data Colorful = Colorful
    { cColor :: Color
    , cText :: Text
    }

instance IsString Colorful where
    fromString = Colorful None . fromString

colorful :: Colorful -> Text
colorfule = undefined
