module Database.Create
    ( dbCreate
    , addingDefault
    , addId
    ) where

import Config
import Data.Base
import Data.Handler
import Data.Essence
import Data.Essence.Methods
import Data.Essence.RelationsTree.Methods
import Data.Essence.Parse.Clause
import Data.MyValue
import Data.SQL
import Data.SQL.Actions
import Data.SQL.ToValue

import Data.Aeson
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import Data.Text.Encoding
    ( encodeUtf8 )
import Data.Time.Clock
import Database.HDBC
    ( disconnect
    , run
    , quickQuery'
    , commit
    , SqlValue ( SqlInteger )
    )
import Database.HDBC.PostgreSQL
    ( connectPostgreSQL
    , Connection
    )

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class          (lift)

dbCreate :: StateT (Essence List) (ReaderT Config IO) Value
dbCreate = do
    config <- lift $ ask
    addingDefault
    essenceList@(EssenceList name action list) <- get
    let createQuery = showSql essenceList
    let uriDB = getUri config
    conn <- lift . lift $ connectPostgreSQL uriDB
    lift . lift $ run conn createQuery []
    lift . lift $ commit conn
    let getQueryId = "SELECT currval('" <> name <> "_id_seq');"
    [[SqlInteger id]] <- lift . lift $ quickQuery' conn getQueryId []
    let getQueryEssence = showSql . Get name $ pickClause name ("id",MyInteger id)
    sqlValues <- lift . lift $ quickQuery' conn getQueryEssence []
    let value = sqlValuesArrToValue essenceList sqlValues config
    lift . lift $ disconnect conn
    pure value

addingDefault :: StateT (Essence List) (ReaderT Config IO) ()
addingDefault = addId

addId :: StateT (Essence List) (ReaderT Config IO) ()
addId = do
    essenceList@(EssenceList name action list) <- get
    let idValue = "nextval('" <> name <> "_id_seq')"
    put $ addList [("id", MyNextval idValue)] essenceList