{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OG.Check where

import Data.Text (Text)
import Data.String (IsString)

-- | Static details about a check, for now just a name
newtype CheckDetail = CheckDetail Text
    deriving IsString

checkLabel :: CheckDetail -> Text
checkLabel (CheckDetail x) = x

data CheckResult a
    = Success a
    | Failure Text
    | Error Text
    | Skipped

instance Functor CheckResult where
    fmap f (Success x) = Success $ f x
    fmap _ (Failure msg) = Failure msg
    fmap _ (Error err) = Failure err
    fmap _ Skipped = Skipped

data Check m a b = Check
    { checkDetail :: CheckDetail
    , checkResult :: a -> m (CheckResult b)
    }

data CheckOutcome a = CheckOutcome
    { coDetail :: CheckDetail
    , coResult :: CheckResult a
    , coDependents :: [CheckOutcome a]
    }

instance Functor CheckOutcome where
    fmap f c = c
        { coResult = f <$> coResult c
        , coDependents = map (f <$>) $ coDependents c
        }
