module Database.Exception
  ( tryConnect
  , tryRun
  , tryQuickQuery
  ) where

import Config
import Data.Base
import Log

import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

tryConnect :: IO PSQL.Connection -> UnderApp (Maybe PSQL.Connection)
tryConnect connectIO = do
  (Config.Handle _ _ logHandle) <- askUnderApp
  result <- liftIO $ tryM connectIO
  case result of
    Right conn ->
      liftIO (debugM logHandle "Database connection is successed") >>
      return (Just conn)
    Left err ->
      liftIO (errorM logHandle "Can't connect to database") >>
      liftIO (errorM logHandle $ show err) >>
      return Nothing

tryRun :: IO Integer -> UnderApp Integer
tryRun run = do
  (Config.Handle _ _ logHandle) <- askUnderApp
  result <- liftIO $ tryM run
  case result of
    Right num ->
      liftIO (debugM logHandle "Query was runned in database") >> return num
    Left err ->
      liftIO (warningM logHandle "Can't run query in database") >>
      liftIO (errorM logHandle $ show err) >>
      return 0

tryQuickQuery :: IO [[HDBC.SqlValue]] -> UnderApp [[HDBC.SqlValue]]
tryQuickQuery quickQuery' = do
  (Config.Handle _ _ logHandle) <- askUnderApp
  result <- liftIO $ tryM quickQuery'
  case result of
    Right arr ->
      liftIO
        (debugM logHandle "Query with getting values was runned in database") >>
      return arr
    Left err ->
      liftIO (warningM logHandle "Can't get essence from database") >>
      liftIO (errorM logHandle $ show err) >>
      return []
