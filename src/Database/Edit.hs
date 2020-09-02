module Database.Edit
  ( dbEdit
  ) where

import Config
import Data.Base
import Data.Essence
import Data.MyValue
import Data.SQL.ShowSql
import Database.Exception
import Log

import qualified Data.Aeson as A
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

dbEdit :: SApp A.Value
dbEdit = do
  essenceList@(EssenceList name _ _) <- getSApp
  (Config.Handle config _ _ logHandle) <- liftUnderApp askUnderApp
  liftUnderApp . liftIO $ debugM logHandle "Start dbEdit"
  let editQuery = showSql essenceList
  let uriDB = getUri config
  maybeConn <- liftUnderApp . tryConnect $ PSQL.connectPostgreSQL uriDB
  case maybeConn of
    Nothing -> return A.Null
    Just conn -> do
      liftUnderApp . liftIO $ HDBC.runRaw conn setEng
      result <- liftUnderApp . tryRun $ HDBC.run conn editQuery []
      case result of
        0 ->
          liftUnderApp . liftIO . infoM logHandle $ name <> " was not changed"
        _ -> liftUnderApp . liftIO . infoM logHandle $ name <> " was changed"
      liftUnderApp . liftIO $ HDBC.commit conn
      let value = A.object ["result" A..= (toValue . MyInteger) result]
      liftUnderApp . liftIO $ HDBC.disconnect conn
      liftUnderApp . liftIO $ debugM logHandle "End dbEdit"
      pure value
