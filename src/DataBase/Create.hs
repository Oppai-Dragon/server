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

import qualified Data.Aeson as A
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

dbCreate :: SApp A.Value
dbCreate = do
  (Config.Handle config _ _) <- liftUnderApp askUnderApp
  addingDefault
  essenceList@(EssenceList name _ _) <- getSApp
  let createQuery = showSql essenceList
  let uriDB = getUri config
  conn <- liftUnderApp . liftIO $ PSQL.connectPostgreSQL uriDB
  _ <- liftUnderApp . liftIO $ HDBC.run conn createQuery []
  liftUnderApp . liftIO $ HDBC.commit conn
  let getQueryId = "SELECT currval('" <> name <> "_id_seq');"
  [[HDBC.SqlInteger num]] <-
    liftUnderApp . liftIO $ HDBC.quickQuery' conn getQueryId []
  let getQueryEssence =
        showSql . Get name $ pickClause name ("id", MyInteger num)
  sqlValues <- liftUnderApp . liftIO $ HDBC.quickQuery' conn getQueryEssence []
  let value = sqlValuesArrToValue essenceList sqlValues config
  liftUnderApp . liftIO $ HDBC.disconnect conn
  pure value

addingDefault :: SApp ()
addingDefault = addId

addId :: SApp ()
addId = do
  (EssenceList name _ _) <- getSApp
  let idValue = "nextval('" <> name <> "_id_seq')"
  modifySApp $ addList [("id", MyNextval idValue)]
