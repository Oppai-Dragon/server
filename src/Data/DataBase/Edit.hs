module DataBase.Edit
    ( dbEdit
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
    ( Action ( editing )
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
    , quickQuery'
    , run
    , commit
    )
import Database.HDBC.PostgreSQL
    ( connectPostgreSQL
    , Connection
    )

dbEdit :: Handler Value
dbEdit = do
    essence <- getEssence
    queryBS <- getQueryBS
    config <- getConfig
    let thing@(Essence name action listOfPairs) = fromQuery essence queryBS config
    let getId = case lookup "id" listOfPairs of
            Just id -> id
            Nothing -> "0"
    let oldThing = Essence name action [("id", getId)]
    let newThing = thing
    let editQuery = case editing oldThing newThing of
            Just str -> str
            Nothing  -> ";"
    let uriDB = getUriDB config
    conn <- fromIO $ connectPostgreSQL uriDB
    result <- fromIO $ run conn editQuery []
    fromIO $ commit conn
    let essenceParsed = ifElseThen [essence=="user"] [essence,"users"]
    let value = object [ "result" .= integerToValue result]
    fromIO $ disconnect conn
    pure value