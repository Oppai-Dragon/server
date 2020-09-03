module Database.Create
  ( dbCreate
  , addingDefault
  , addId
  ) where

import Config
import Data.Base
import Data.Essence
import Data.Essence.Methods
import Data.Essence.Parse.Clause
import Data.MyValue
import Data.SQL
import Data.SQL.ShowSql
import Data.SQL.ToValue
import Database.Exception
import Log

import qualified Data.Aeson as A
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

dbCreate :: SApp A.Value
dbCreate = do
  (Config.Handle config _ _ logHandle) <- liftUnderApp askUnderApp
  liftUnderApp . liftIO $ debugM logHandle "Start dbCreate"
  addingDefault
  essenceList@(EssenceList name _ _) <- getSApp
  let createQuery = showSql essenceList
  let uriDB = getUri config
  maybeConn <- liftUnderApp . tryConnect $ PSQL.connectPostgreSQL uriDB
  case maybeConn of
    Nothing ->
      liftIO (debugM logHandle "Essence not created in database") >>
      return A.Null
    Just conn -> do
      liftUnderApp . liftIO $ HDBC.runRaw conn setEng
      liftUnderApp . liftIO . debugM logHandle $ "PSQL request: " <> createQuery
      result <- liftUnderApp . tryRun $ HDBC.run conn createQuery []
      case result of
        0 -> liftUnderApp . liftIO . infoM logHandle $ name <> " wasn't created"
        _ -> liftUnderApp . liftIO . infoM logHandle $ name <> " was created"
      liftUnderApp . liftIO $ HDBC.commit conn
      let getQueryId = "SELECT currval('" <> name <> "_id_seq');"
      idSqlValues <-
        liftUnderApp . tryQuickQuery $ HDBC.quickQuery' conn getQueryId []
      let getQueryEssence =
            case idSqlValues of
              [[HDBC.SqlInteger num]] ->
                showSql . Get name $ pickClause name ("id", MyInteger num)
              _ -> ""
      sqlValues <-
        liftUnderApp . tryQuickQuery $ HDBC.quickQuery' conn getQueryEssence []
      let value = sqlValuesArrToValue essenceList sqlValues config
      liftUnderApp . liftIO $ HDBC.disconnect conn
      liftUnderApp . liftIO $ debugM logHandle "End dbCreate"
      pure value

addingDefault :: SApp ()
addingDefault = addId

addId :: SApp ()
addId = do
  (EssenceList name _ _) <- getSApp
  let idValue = "nextval('" <> name <> "_id_seq')"
  modifySApp $ addList [("id", MyNextval idValue)]
