module DataBase.Edit
    ( dbEdit
    ) where

import Config
import Data.Base
    ( ifElseThen )
import Data.Handler
import Data.Essence
import Data.Essence.Methods
import Data.SQL.Actions
import Data.SQL.ToValue

import Data.Aeson
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

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class          (lift)

dbEdit :: StateT (Essence List) (ReaderT Config IO) Value
dbEdit = do
    essenceList <- get
    config <- lift ask
    let editQuery = show essenceList
    let uriDB = getUri config
    conn <- lift . lift $ connectPostgreSQL uriDB
    result <- lift . lift $ run conn editQuery []
    lift . lift $ commit conn
    let value = object [ "result" .= integerToValue result]
    lift . lift $ disconnect conn
    pure value