module Database.Delete
  ( dbDelete
  ) where

import Config
import Data.Essence
import Data.MyValue
import Data.SQL.ShowSql

import qualified Data.Aeson as A
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

dbDelete :: StateT (Essence List) (ReaderT Config IO) A.Value
dbDelete = do
  essenceList <- get
  config <- lift ask
  let deleteQuery = showSql essenceList
  let uriDB = getUri config
  conn <- lift . lift $ PSQL.connectPostgreSQL uriDB
  result <- lift . lift $ HDBC.run conn deleteQuery []
  lift . lift $ HDBC.commit conn
  let value = A.object ["result" A..= (toValue . MyInteger) result]
  lift . lift $ HDBC.disconnect conn
  pure value
