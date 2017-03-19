{-# LANGUAGE RecordWildCards #-}
module OG.Check
    ( CheckDetails(..)
    , CheckOutcome(..)
    , CheckResult(..)
    , Check(..)
    , runCheck
    , skipCheck
    ) where

import Data.Text (Text)

import qualified Control.Exception.Safe as E
import qualified Data.Text as T

data CheckDetails = CheckDetails
    { cdName :: Text
    , cdDescription :: Text
    }

data CheckOutcome
    = Success
    | Failure
    | Error Text
    | Skipped Text

data Check m = Check
    { checkDetails :: CheckDetails
    , checkOutcome :: m CheckOutcome
    }

data CheckResult = CheckResult
    { crDetails :: CheckDetails
    , crOutcome :: CheckOutcome
    }

runCheck :: E.MonadCatch m => Check m -> m CheckResult
runCheck Check{..} = CheckResult checkDetails <$> do
    outcome <- E.tryIO checkOutcome

    return $ either (Error . T.pack . show) id outcome

skipCheck :: Monad m => Text -> Check m -> m CheckResult
skipCheck reason check = return $ CheckResult
    { crDetails = checkDetails check
    , crOutcome = Skipped reason
    }
