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
  Config.Handle {hLogHandle=logHandle} <- askUnderApp
  result <- liftIO $ tryM connectIO
  case result of
    Right conn ->
      liftIO (logDebug logHandle "Database connection is successed") >>
      return (Just conn)
    Left err ->
      liftIO (logError logHandle "Can't connect to database") >>
      liftIO (logError logHandle $ show err) >>
      return Nothing

tryRun :: HasCallStack => IO a -> UnderApp Result
tryRun run = do
  Config.Handle {hLogHandle=logHandle} <- askUnderApp
  result <- liftIO $ tryM run
  case result of
    Right _ ->
      liftIO (logDebug logHandle "Query was runned in database") >> return Success
    Left err ->
      liftIO (logWarning logHandle "Can't run query in database") >>
      liftIO (logError logHandle $ show err) >>
      return Fail

tryQuickQuery ::
     HasCallStack => IO [[HDBC.SqlValue]] -> UnderApp [[HDBC.SqlValue]]
tryQuickQuery quickQuery' = do
  Config.Handle {hLogHandle=logHandle} <- askUnderApp
  result <- liftIO $ tryM quickQuery'
  case result of
    Right arr ->
      liftIO
        (logDebug logHandle "Query with getting values was runned in database") >>
      return arr
    Left err ->
      liftIO (logWarning logHandle "Can't get query from database") >>
      liftIO (logError logHandle $ show err) >>
      return []

tryConnectIO :: HasCallStack => IO PSQL.Connection -> IO (Maybe PSQL.Connection)
tryConnectIO connectIO = do
  logHandle <- Log.new
  result <- tryM connectIO
  case result of
    Right conn ->
      logDebug logHandle "Database connection is successed" >> return (Just conn)
    Left err ->
      logError logHandle "Can't connect to database" >>
      logError logHandle (show err) >>
      return Nothing

tryRunIO :: HasCallStack => IO a -> IO Result
tryRunIO run = do
  logHandle <- Log.new
  result <- tryM run
  case result of
    Right _ -> logDebug logHandle "Query was runned in database" >> return Success
    Left err ->
      logWarning logHandle "Can't run query in database" >>
      logError logHandle (show err) >>
      return Fail

tryQuickQueryIO :: HasCallStack => IO [[HDBC.SqlValue]] -> IO [[HDBC.SqlValue]]
tryQuickQueryIO quickQuery' = do
  logHandle <- Log.new
  result <- tryM quickQuery'
  case result of
    Right arr ->
      logDebug logHandle "Query with getting values was runned in database" >>
      return arr
    Left err ->
      logWarning logHandle "Can't get query from database" >>
      logError logHandle (show err) >>
      return []
