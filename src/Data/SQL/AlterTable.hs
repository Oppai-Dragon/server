module Data.SQL.AlterTable
  (AlterTable(..),getAlterQueries) where

import Data.Essence.Column

import qualified Data.Text as T

type SqlQuery = String

type Table = String

type ColumnName = String

class AlterTable a where
  alterTable :: Table -> ColumnName -> a -> SqlQuery
instance AlterTable ValueType where
  alterTable table column x = "ALTER TABLE " <> table <> " ALTER COLUMN " <> column <> " SET DATA TYPE " <> show x <> ";"
instance AlterTable (Maybe NULL) where
  alterTable table column (Just $ NOT NULL) = "ALTER TABLE " <> table <> " ALTER COLUMN " <> column <> " SET NOT NULL;"
  alterTable table column _ = "ALTER TABLE " <> table <> " ALTER COLUMN " <> column <> " DROP NOT NULL;"
instance AlterTable (Maybe Default) where
  alterTable table column (Just $ Default var) = "ALTER TABLE " <> table <> " ALTER COLUMN " <> column <> " SET DEFAULT " <> T.unpack text <> ";"

getAlterQueries :: Table -> ColumnName -> ColumnLocal -> ColumnDatabase -> IO String
getAlterQueries table columnName columnLoc columnDb =