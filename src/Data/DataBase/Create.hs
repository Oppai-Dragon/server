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
import DataBase.Types
    ( EssenceApi
    , QueryBS
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

dbCreate :: Handler Value
dbCreate = do
    essence <- getEssence
    queryBS <- getQueryBS
    config <- getConfig
    let essenceStr = unpack essence
    newQueryBS <- fromIO $ getUpdatedQueryBS essence queryBS config
    let thing@(Essence name action listOfPairs) = fromQuery essence newQueryBS config
    let createQuery = case creating thing of
            Just str -> str
            Nothing  -> ";"
    let uriDB = getUriDB config
    conn <- fromIO $ connectPostgreSQL uriDB
    fromIO $ run conn createQuery []
    fromIO $ commit conn
    let getQueryId = "SELECT currval('" <> essenceStr <> "_id_seq');"
    [[SqlInteger idEssence]] <- fromIO $ quickQuery' conn getQueryId []
    let getQueryEssence =
            case getting $ Essence essenceStr [("id",show idEssence)] of
                Just query -> query
                Nothing    -> ";"
    sqlValues <- fromIO $ quickQuery' conn getQueryEssence []
    let essence' = ifElseThen [essence == "user"] [essence,"oneUser"]
    let value = sqlValuesArrToValue essence' sqlValues config
    fromIO $ disconnect conn
    pure value

getUpdatedQueryBS :: EssenceApi -> QueryBS -> Config -> IO QueryBS
getUpdatedQueryBS essence queryBS conf = do
    date <- getCurrentTime
    let essenceParsed = ifElseThen [essence=="user"] [essence,"users"]
    let dateBS = encodeUtf8 $ pack $ show date
    let idValueT = "nextval('" <> essenceParsed <> "_id_seq')"
    let idValueBS = encodeUtf8 idValueT
    let addDateAndId = (<>)
            [("date_of_creation", Just dateBS)
            ,("id", Just idValueBS)
            ]
    let resultQueryBS = addDateAndId queryBS
    pure resultQueryBS