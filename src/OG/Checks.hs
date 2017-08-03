{-# LANGUAGE OverloadedStrings #-}
module OG.Checks
    ( checks
    ) where

import OG.Check
import OG.Check.DSL
import OG.Checks.Support

import Control.Monad.IO.Class (MonadIO)

checks :: MonadIO m => m [CheckOutcome ()]
checks = sequence
    [ runIndependent "LICENSE present" $ pathExists "LICENSE"
    , runDependents (independent "README present" $ pathExists "README*")
        [ dependent "README Usage" (`hasLine` "## Usage")
        , dependent "README Development & Test" (`hasLine` "## Development & Test")
        ]
    , runDependents (independent "Makefile present" $ pathExists "Makefile")
        [ dependent "Makefile test target" (`hasLine` "test:")
        ]
    , runDependents (independent "CHANGELOG present" $ pathExists "CHANGELOG*")
        [ dependent "CHANGELOG Unreleased" (`hasLine` "## [Unreleased]")
        ]
    ]
