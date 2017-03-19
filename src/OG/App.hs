{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module OG.App
    ( app
    ) where

import OG.Options

import Control.Monad.Except

import qualified Data.Vector as V
import qualified GitHub.Endpoints.Repos as GH

app :: Options -> IO (Either GH.Error ())
app Options{..} = runExceptT $ do
    GH.ContentDirectory items <-
        ExceptT $ GH.contentsFor (toName oOrg) (toName oRepo) "" Nothing

    liftIO $ print $ V.toList $ GH.contentPath . GH.contentItemInfo <$> items
