module Database.Delete
  ( dbDelete
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

dbDelete :: SApp A.Value
dbDelete = do
  essenceList@(EssenceList name _ _) <- getSApp
  (Config.Handle config _ _ logHandle) <- liftUnderApp askUnderApp
  liftUnderApp . liftIO $ debugM logHandle "Start dbDelete"
  let deleteQuery = showSql essenceList
  let uriDB = getUri config
  maybeConn <- liftUnderApp . tryConnect $ PSQL.connectPostgreSQL uriDB
  case maybeConn of
    Nothing -> return A.Null
    Just conn -> do
      liftUnderApp . liftIO $ HDBC.runRaw conn setEng
      liftUnderApp . liftIO . debugM logHandle $ "PSQL request: " <> deleteQuery
      result <- liftUnderApp . tryRun $ HDBC.run conn deleteQuery []
      case result of
        0 ->
          liftUnderApp . liftIO . infoM logHandle $ name <> " was not deleted"
        _ -> liftUnderApp . liftIO . infoM logHandle $ name <> " was deleted"
      liftUnderApp . liftIO $ HDBC.commit conn
      let value = A.object ["result" A..= (toValue . MyInteger) result]
      liftUnderApp . liftIO $ HDBC.disconnect conn
      liftUnderApp . liftIO $ debugM logHandle "End dbDelete"
      pure value
