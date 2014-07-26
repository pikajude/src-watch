{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Watch.Logger where

import Control.Monad.Reader

data Verbosity = Loud | Quiet
               deriving (Bounded, Eq, Enum, Ord, Show)

type Logger m = ReaderT Verbosity m

runLogger :: Verbosity -> Logger m a -> m a
runLogger v = (`runReaderT` v)

loud, quiet :: (MonadReader Verbosity m, MonadIO m) => IO () -> m ()
loud = help Loud
quiet = help Quiet

help :: (MonadReader a m, MonadIO m, Ord a) => a -> IO () -> m ()
help t m = ask >>= \ l -> when (l <= t) (liftIO m)
