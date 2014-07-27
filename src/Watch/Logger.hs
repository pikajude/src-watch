{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Watch.Logger (
    Verbosity (..),
    runLogger,
    loud, quiet, deafening
) where

import Control.Monad.Reader

data Verbosity = Quiet | Loud | Deafening
               deriving (Bounded, Eq, Enum, Ord, Show)

type Logger m = ReaderT Verbosity m

runLogger :: Verbosity -> Logger m a -> m a
runLogger v = (`runReaderT` v)

deafening, loud, quiet :: (MonadReader Verbosity m, MonadIO m) => IO () -> m ()
deafening = help Deafening
loud = help Loud
quiet = help Quiet

help :: (MonadReader a m, MonadIO m, Ord a) => a -> IO () -> m ()
help t m = ask >>= \ l -> when (t <= l) (liftIO m)
