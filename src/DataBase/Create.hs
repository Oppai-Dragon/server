module DataBase.Create
    ( dbCreate
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
    --addingRelationsFields
    essenceList@(EssenceList name action list) <- get
    let createQuery = showSql essenceList
    let uriDB = getUri config
    let essence = T.pack name
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

--addingRelationsFields :: StateT (Essence List) (ReaderT Config IO) ()
--addingRelationsFields = do
--    config <- lift ask
--    essenceList@(EssenceList name action list) <- get
--    let essenceDB = getEssenceDB (T.pack name) (T.pack action) config
--    relationsFields <- getRelationsFields name
--    if isRelationsFieldsNeeded essenceDB
--        then put $ addList relationsFields essenceList
--        else put essenceList

addingDefault :: StateT (Essence List) (ReaderT Config IO) ()
addingDefault = do
    config <- lift ask
    api <- lift . lift $ setApi
    essenceList@(EssenceList name action list) <- get
    let essenceDB = getEssenceDB (T.pack name) (T.pack action) config api
    let isDateOfCreation = case HM.lookup "date_of_creation" (fieldsOf essenceDB) of
            Just _  -> True
            Nothing -> False
    let idValue = "nextval('" <> name <> "_id_seq')"
    date <- lift . lift $ getCurrentTime
    let dateValue = show $ utctDay date
    if isDateOfCreation
        then put $ addList [("id", MyNextval idValue),("date_of_creation", MyDate dateValue)] essenceList
        else put $ addList [("id", MyNextval idValue)] essenceList