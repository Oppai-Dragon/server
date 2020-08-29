module Database.Edit
  ( dbEdit
  ) where

import Config
import Data.Base
import Data.MyValue
import Data.SQL.ShowSql

import qualified Data.Aeson as A
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

dbEdit :: SApp A.Value
dbEdit = do
  essenceList <- getSApp
  (Config.Handle config _ _) <- liftUnderApp askUnderApp
  let editQuery = showSql essenceList
  let uriDB = getUri config
  conn <- liftUnderApp . liftIO $ PSQL.connectPostgreSQL uriDB
  result <- liftUnderApp . liftIO $ HDBC.run conn editQuery []
  liftUnderApp . liftIO $ HDBC.commit conn
  let value = A.object ["result" A..= (toValue . MyInteger) result]
  liftUnderApp . liftIO $ HDBC.disconnect conn
  pure value
