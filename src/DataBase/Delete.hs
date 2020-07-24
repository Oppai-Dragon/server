module DataBase.Delete
    ( dbDelete
    ) where

import Config
import Data.Base
import Data.Handler
import Data.Essence
import Data.Essence.Methods
import Data.SQL.ToValue
import Data.SQL.Actions
import Data.Aeson
import Database.HDBC
    ( disconnect
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

dbDelete :: StateT (Essence List) (ReaderT Config IO) Value
dbDelete = do
    essenceList <- get
    config <- lift ask
    let deleteQuery = show essenceList
    let uriDB = getUri config
    conn <- lift . lift $ connectPostgreSQL uriDB
    result <- lift . lift $ run conn deleteQuery []
    lift . lift $ commit conn
    let value = object [ "result" .= integerToValue result]
    lift . lift $ disconnect conn
    pure value