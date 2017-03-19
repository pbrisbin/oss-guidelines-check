{-# LANGUAGE OverloadedStrings #-}
module OG.Options
    ( ToName(..)
    , Options(..)
    , Org(..)
    , Repo(..)
    , parseOptions
    ) where

import Data.Text (Text)

import qualified GitHub.Data.Name as GH

data Options = Options
    { oOrg :: Org
    , oRepo :: Repo
    }

newtype Org = Org Text
newtype Repo = Repo Text

class ToName a where
    toName :: a -> GH.Name e

instance ToName Org where
    toName (Org t) = GH.N t

instance ToName Repo where
    toName (Repo t) = GH.N t

parseOptions :: IO Options
parseOptions = return Options
    { oOrg = Org "pbrisbin"
    , oRepo = Repo "aurget"
    }
