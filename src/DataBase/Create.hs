module Database.Create
  ( dbCreate
  , addingDefault
  , addId
  ) where

import Config
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

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

dbCreate :: StateT (Essence List) (ReaderT Config IO) A.Value
dbCreate = do
  config <- lift ask
  addingDefault
  essenceList@(EssenceList name _ _) <- get
  let createQuery = showSql essenceList
  let uriDB = getUri config
  conn <- lift . lift $ PSQL.connectPostgreSQL uriDB
  _ <- lift . lift $ HDBC.run conn createQuery []
  lift . lift $ HDBC.commit conn
  let getQueryId = "SELECT currval('" <> name <> "_id_seq');"
  [[HDBC.SqlInteger num]] <- lift . lift $ HDBC.quickQuery' conn getQueryId []
  let getQueryEssence =
        showSql . Get name $ pickClause name ("id", MyInteger num)
  sqlValues <- lift . lift $ HDBC.quickQuery' conn getQueryEssence []
  let value = sqlValuesArrToValue essenceList sqlValues config
  lift . lift $ HDBC.disconnect conn
  pure value

addingDefault :: StateT (Essence List) (ReaderT Config IO) ()
addingDefault = addId

addId :: StateT (Essence List) (ReaderT Config IO) ()
addId = do
  (EssenceList name _ _) <- get
  let idValue = "nextval('" <> name <> "_id_seq')"
  modify $ addList [("id", MyNextval idValue)]
