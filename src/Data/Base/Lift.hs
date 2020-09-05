module Data.Base.Lift
  ( liftApp
  , liftUnderApp
  , liftIO
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

liftApp :: (MonadTrans t, Monad m) => StateT s m a -> t (StateT s m) a
liftApp = lift

liftUnderApp :: (MonadTrans t) => ReaderT r IO a -> t (ReaderT r IO) a
liftUnderApp = lift
