module Database.Edit
  ( dbEdit
  ) where

import Config
import Data.Base
import Data.Essence
import Data.SQL.ShowSql
import Database.Exception
import Log

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

dbEdit :: SApp A.Value
dbEdit = do
  essenceList@(EssenceList {elName = name}) <- getSApp
  Config.Handle {hConfig = config, hLogHandle = logHandle} <-
    liftUnderApp askUnderApp
  liftUnderApp . liftIO $ logDebug logHandle "Start dbEdit"
  let editQuery = showSql essenceList
  let uriDB = getUri config
  maybeConn <- liftUnderApp . tryConnect $ PSQL.connectPostgreSQL uriDB
  case maybeConn of
    Nothing -> return A.Null
    Just conn -> do
      liftUnderApp . liftIO $ HDBC.runRaw conn setEng
      liftUnderApp . liftIO . logDebug logHandle $ "PSQL request: " <> editQuery
      result <- liftUnderApp . tryRun $ HDBC.run conn editQuery []
      case result of
        Success ->
          liftUnderApp . liftIO . logInfo logHandle $ name <> " was not changed"
        Fail ->
          liftUnderApp . liftIO . logInfo logHandle $ name <> " was changed"
      liftUnderApp . liftIO $ HDBC.commit conn
      let value = A.object ["result" A..= (A.String . T.pack . show) result]
      liftUnderApp . liftIO $ HDBC.disconnect conn
      liftUnderApp . liftIO $ logDebug logHandle "End dbEdit"
      pure value
