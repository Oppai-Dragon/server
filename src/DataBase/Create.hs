module DataBase.Create
    ( dbCreate
    ) where

import Config
    ( Config
    , getUriDB
    )
import Data.Base
    ( ifElseThen )
import Data.Handler
import Data.Essence
    ( Essence (..) )
import Data.Essence.Parse
    ( fromQuery )
import Data.Essence.Relations.Methods
    ( isEssenceRelations
    , getRelationsFields
    )
import Data.SQL.Actions
    ( Action ( creating, getting ) )
import Data.SQL.ToValue
    ( integerToValue
    , sqlValuesArrToValue
    )

import Data.Aeson
    ( Value )
import Data.Text
    ( unpack
    , pack
    )
import Data.Text.Encoding
    ( encodeUtf8 )
import Data.Time.Clock
    ( getCurrentTime )
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
    addingRelationsFields
    essenceList@(EssenceList name action list) <- get
    let createQuery = show essenceList
    let uriDB = getUri config
    let essence = T.pack $ name essenceList
    conn <- lift . lift $ connectPostgreSQL uriDB
    lift . lift $ run conn createQuery []
    lift . lift $ commit conn
    let getQueryId = "SELECT currval('" <> name <> "_id_seq');"
    [[SqlInteger idEssence]] <- lift . lift $ quickQuery' conn getQueryId []
    let getQueryEssence = show (Essence name "get" [("id",show idEssence)])
    sqlValues <- lift . lift $ quickQuery' conn getQueryEssence []
    let value = sqlValuesArrToValue essenceList sqlValues config
    lift . lift $ disconnect conn
    pure value

addingRelationsFields :: StateT (Essence List) (ReaderT Config IO) ()
addingRelationsFields =
    config <- lift ask
    essenceList@(EssenceList name action list) <- get
    let essenceDB = getEssenceDB (T.pack name) (T.pack action) config
    relationsFields <- getRelationsFields name
    if isRelationsFieldsNeeded essenceDB
        then put $ addList relationsFields essenceList
        else put essenceList

addingDefault :: StateT (Essence List) (ReaderT Config IO) ()
addingDefault = do
    config <- lift ask
    essenceList@(EssenceList name action list) <- get
    let essenceDB = getEssenceDB (T.pack name) (T.pack action) config
    let isDateOfCreation = case HM.lookup "date_of_creation" (fieldsOf essenceDB) of
            Just _  -> True
            Nothing -> False
    let idValue = "nextval('" <> name <> "_id_seq')"
    date <- getCurrentTime
    let dateValue = show $ utctDay date
    if isDateOfCreation
        then put $ addList [("id",idValue),("date_of_creation",dateValue)] essenceList
        else put $ addList [("id",idValue)] essenceList