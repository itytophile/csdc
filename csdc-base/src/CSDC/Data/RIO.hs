{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CSDC.Data.RIO
  ( RIO,
    runRIO,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState (..))
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, readIORef)

newtype RIO s m a = RIO (ReaderT (IORef s) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runRIO :: MonadIO m => IORef s -> RIO s m a -> m a
runRIO var (RIO m) = runReaderT m var

instance MonadIO m => MonadState s (RIO s m) where
  get = RIO $ do
    var <- ask
    liftIO $ readIORef var

  put !x = RIO $ do
    var <- ask
    liftIO $ atomicWriteIORef var x

  state f = RIO $ do
    var <- ask
    let f' !x = let !(!a, !b) = f x in (b, a)
    liftIO $ atomicModifyIORef' var f'
