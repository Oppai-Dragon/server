module Database.Exception
  ( Result(..)
  , tryConnect
  , tryRun
  , tryQuickQuery
  , tryConnectIO
  , tryRunIO
  , tryQuickQueryIO
  ) where

import Config
import Data.Base
import Log

import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

data Result
  = Success
  | Fail
  deriving (Show, Eq)

tryConnect ::
     HasCallStack => IO PSQL.Connection -> UnderApp (Maybe PSQL.Connection)
tryConnect connectIO = do
  (Config.Handle _ _ _ logHandle) <- askUnderApp
  result <- liftIO $ tryM connectIO
  case result of
    Right conn ->
      liftIO (debugM logHandle "Database connection is successed") >>
      return (Just conn)
    Left err ->
      liftIO (errorM logHandle "Can't connect to database") >>
      liftIO (errorM logHandle $ show err) >>
      return Nothing

tryRun :: HasCallStack => IO a -> UnderApp Result
tryRun run = do
  (Config.Handle _ _ _ logHandle) <- askUnderApp
  result <- liftIO $ tryM run
  case result of
    Right _ ->
      liftIO (debugM logHandle "Query was runned in database") >> return Success
    Left err ->
      liftIO (warningM logHandle "Can't run query in database") >>
      liftIO (errorM logHandle $ show err) >>
      return Fail

tryQuickQuery ::
     HasCallStack => IO [[HDBC.SqlValue]] -> UnderApp [[HDBC.SqlValue]]
tryQuickQuery quickQuery' = do
  (Config.Handle _ _ _ logHandle) <- askUnderApp
  result <- liftIO $ tryM quickQuery'
  case result of
    Right arr ->
      liftIO
        (debugM logHandle "Query with getting values was runned in database") >>
      return arr
    Left err ->
      liftIO (warningM logHandle "Can't get query from database") >>
      liftIO (errorM logHandle $ show err) >>
      return []

tryConnectIO :: HasCallStack => IO PSQL.Connection -> IO (Maybe PSQL.Connection)
tryConnectIO connectIO = do
  logHandle <- Log.new
  result <- tryM connectIO
  case result of
    Right conn ->
      debugM logHandle "Database connection is successed" >> return (Just conn)
    Left err ->
      errorM logHandle "Can't connect to database" >>
      errorM logHandle (show err) >>
      return Nothing

tryRunIO :: HasCallStack => IO a -> IO Result
tryRunIO run = do
  logHandle <- Log.new
  result <- tryM run
  case result of
    Right _ -> debugM logHandle "Query was runned in database" >> return Success
    Left err ->
      warningM logHandle "Can't run query in database" >>
      errorM logHandle (show err) >>
      return Fail

tryQuickQueryIO :: HasCallStack => IO [[HDBC.SqlValue]] -> IO [[HDBC.SqlValue]]
tryQuickQueryIO quickQuery' = do
  logHandle <- Log.new
  result <- tryM quickQuery'
  case result of
    Right arr ->
      debugM logHandle "Query with getting values was runned in database" >>
      return arr
    Left err ->
      warningM logHandle "Can't get query from database" >>
      errorM logHandle (show err) >>
      return []
