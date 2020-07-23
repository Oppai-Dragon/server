module DataBase.Delete
    ( dbDelete
    ) where

import Config
    ( Config
    , getUriDB
    )
import Data.Base
    ( ifElseThen )
import Data.Handler
import Data.Essence
    ( Essence ( Essence )
    )
import Data.Essence.Parse
    ( fromQuery
    )
import Data.SQL.Actions
    ( Action ( deleting )
    )
import Data.SQL.ToValue
    ( integerToValue
    )
import DataBase.Types
    ( EssenceApi
    , QueryBS
    )

import Data.Aeson
    ( Value
    , object
    , (.=)
    )
import Database.HDBC
    ( disconnect
    , run
    , commit
    )
import Database.HDBC.PostgreSQL
    ( connectPostgreSQL
    , Connection
    )

dbDelete :: Handler Value
dbDelete = do
    essence <- getEssence
    queryBS <- getQueryBS
    config <- getConfig
    let thing@(Essence name action listOfPairs) = fromQuery essence queryBS config
    let deleteQuery = case deleting thing of
            Just str -> str
            Nothing  -> ";"
    let uriDB = getUriDB config
    conn <- fromIO $ connectPostgreSQL uriDB
    result <- fromIO $ run conn deleteQuery []
    fromIO $ commit conn
    let essenceParsed = ifElseThen [essence=="user"] [essence,"users"]
    let value = object [ "result" .= integerToValue result]
    fromIO $ disconnect conn
    pure value