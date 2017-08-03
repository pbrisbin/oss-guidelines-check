{-# LANGUAGE RecordWildCards #-}
module OG.Check.DSL
    ( independent
    , dependent
    , runIndependent
    , runDependents
    )
    where

import OG.Check

import Control.Monad (void)

independent :: CheckDetail -> m (CheckResult b) -> Check m a b
independent d f = Check d (\_ -> f)

runIndependent :: Functor m => CheckDetail -> m (CheckResult b) -> m (CheckOutcome ())
runIndependent d = run_ () . independent d

dependent :: CheckDetail -> (a -> m (CheckResult b)) -> Check m a b
dependent = Check

runDependents :: Monad m => Check m () a -> [Check m a b] -> m (CheckOutcome ())
runDependents a bs = do
    r <- coResult <$> run () a
    os <- case r of
        Success x -> mapM (run_ x) bs
        _ -> return $ map skip bs

    return $ CheckOutcome (checkDetail a) (void r) os

run :: Functor m => a -> Check m a b -> m (CheckOutcome b)
run a Check{..} = (\r -> CheckOutcome checkDetail r []) <$> checkResult a

run_ :: Functor m => a -> Check m a b -> m (CheckOutcome ())
run_ a = (void <$>) . run a

skip :: Check m a b -> CheckOutcome c
skip Check{..} = CheckOutcome checkDetail Skipped []
