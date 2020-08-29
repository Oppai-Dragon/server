module Database.Delete
  ( dbDelete
  ) where

import Config
import Data.Base
import Data.MyValue
import Data.SQL.ShowSql

import qualified Data.Aeson as A
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

dbDelete :: SApp A.Value
dbDelete = do
  essenceList <- getSApp
  (Config.Handle config _ _) <- liftUnderApp askUnderApp
  let deleteQuery = showSql essenceList
  let uriDB = getUri config
  conn <- liftUnderApp . liftIO $ PSQL.connectPostgreSQL uriDB
  result <- liftUnderApp . liftIO $ HDBC.run conn deleteQuery []
  liftUnderApp . liftIO $ HDBC.commit conn
  let value = A.object ["result" A..= (toValue . MyInteger) result]
  liftUnderApp . liftIO $ HDBC.disconnect conn
  pure value
