module Data.Base.Interface
  ( askUnderApp
  , getSApp
  , putSApp
  , tellWApp
  , runUnderApp
  , modifySApp
  , evalSApp
  , execSApp
  , runSApp
  , evalWApp
  , execWApp
  , runWApp
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS

askUnderApp :: Monad m => ReaderT r m r
askUnderApp = ask

getSApp :: Monad m => StateT s m s
getSApp = get

putSApp :: Monad m => s -> StateT s m ()
putSApp = put

tellWApp :: (Monoid w, Monad m) => w -> WriterT w m ()
tellWApp = tell

runUnderApp :: Monad m => ReaderT b m a -> b -> m a
runUnderApp = runReaderT

modifySApp :: Monad m => (s -> s) -> StateT s m ()
modifySApp = modify

evalSApp :: Monad m => StateT s m a -> s -> m a
evalSApp = evalStateT

execSApp :: Monad m => StateT s m a -> s -> m s
execSApp = execStateT

runSApp :: Monad m => StateT s m a -> s -> m (a, s)
runSApp = runStateT

evalWApp :: (Monoid w, Monad m) => WriterT w m a -> m a
evalWApp x = runWriterT x >>= \(a, _) -> return a

execWApp :: (Monoid w, Monad m) => WriterT w m a -> m w
execWApp = execWriterT

runWApp :: (Monoid w, Monad m) => WriterT w m a -> m (a, w)
runWApp = runWriterT
