module Database.Update
  ( updateDatabase
  ) where

import Config
import Config.Exception
import Data.Base
import Database.Exception
import Log
import Setup

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Database.HDBC as HDBC

type Table = String

type EssenceLocal = String

type TableObj = A.Object

type ColumnLocal = T.Text

type ColumnArr = [T.Text]

type ColumnDatabase = T.Text

type ColumnList = [(T.Text, A.Value)]

type ColumnLocalList = [(T.Text, A.Value)]

type ColumnLocalObj = A.Object

type ColumnDatabaseObj = A.Object

type EssenceDatabase = String

type ColumnDatabaseList = [(T.Text, A.Value)]

type EssenceApi = String

--type EssenceNew = [String]
updateDatabase :: IO ()
updateDatabase = do
  essences <- map T.unpack . getEssences <$> setApi
  essenceDatabaseArr1 <- getEssenceArr "EssenceDatabase"
  essenceLocalArr1 <- getEssenceArr "EssenceLocal"
  if isRedundantTables essences essenceDatabaseArr1 essenceLocalArr1
    then dropTables $ essenceDatabaseArr1 \\ essenceLocalArr1
    else infoIO "No need delete tables"
  if isNewTables essences essenceDatabaseArr1 essenceLocalArr1
    then createTables $ essenceLocalArr1 \\ essenceDatabaseArr1
    else infoIO "No need add tables"
  essenceDatabaseArr2 <- getEssenceArr "EssenceDatabase"
  essenceLocalArr2 <- getEssenceArr "EssenceLocal"
  changedTables <-
    getChangedTables (sort essenceDatabaseArr2) (sort essenceLocalArr2)
  if null changedTables
    then infoIO "No need change tables"
    else updateTableArr changedTables
  buildConfigJson

isRedundantTables, isNewTables ::
     [EssenceApi] -> [EssenceDatabase] -> [EssenceLocal] -> Bool
isRedundantTables apiArr dbArr locArr =
  null (apiArr \\ locArr) && (not . null) (dbArr \\ locArr)

isNewTables apiArr dbArr locArr =
  null (apiArr \\ locArr) && (not . null) (locArr \\ dbArr)

getChangedTables :: [EssenceDatabase] -> [EssenceLocal] -> IO [EssenceLocal]
getChangedTables (essenceDb:restDb) (essenceLoc:restLoc) = do
  essenceDbObj <- set =<< setPath ("EssenceDatabase\\" <> essenceDb <> ".json")
  essenceLocObj <- set =<< setPath ("EssenceLocal\\" <> essenceLoc <> ".json")
  if essenceDbObj == essenceLocObj
    then getChangedTables restDb restLoc
    else (essenceLoc :) <$> getChangedTables restDb restLoc
getChangedTables _ _ = return []

updateTableArr :: [EssenceLocal] -> IO ()
updateTableArr [] = return ()
updateTableArr (essence:rest) = do
  updateTable essence
  updateTableArr rest

updateTable :: EssenceLocal -> IO ()
updateTable essence = do
  essenceLocObj <- set =<< setPath ("EssenceLocal\\" <> essence <> ".json")
  essenceDbObj <- set =<< setPath ("EssenceDatabase\\" <> essence <> ".json")
  let columnsDbList = toColumnList essence essenceLocObj
  let columnDbArr = getColumnArr essence essenceLocObj
  let columnsLocList = toColumnList essence essenceDbObj
  let columnLocArr = getColumnArr essence essenceDbObj
  if isRedundantColumns columnDbArr columnLocArr
    then dropColumns essence $ columnsDbList \\ columnsLocList
    else infoIO "No need delete columns"
  if isNewColumns columnDbArr columnLocArr
    then createColumns essence $ columnsLocList \\ columnsDbList
    else infoIO "No need add columns"
  changedColumnList <- getChangedColumnList essence
  if null changedColumnList
    then infoIO "No need change columns"
    else updateColumnArr essence changedColumnList

isRedundantColumns, isNewColumns :: [ColumnDatabase] -> [ColumnLocal] -> Bool
isRedundantColumns = ((not . null) .) . (\\)

isNewColumns = ((not . null) .) . flip (\\)

getChangedColumnList :: Table -> IO ColumnLocalList
getChangedColumnList table = do
  essenceDbObj <- set =<< setPath ("EssenceDatabase\\" <> table <> ".json")
  essenceLocObj <- set =<< setPath ("EssenceLocal\\" <> table <> ".json")
  let columnDbList = toColumnList table essenceDbObj
  let columnLocList = toColumnList table essenceLocObj
  return $ getChangedColumnList' columnDbList columnLocList

getChangedColumnList' ::
     ColumnDatabaseList -> ColumnLocalList -> ColumnLocalList
getChangedColumnList' (columnDbPair:restDb) (columnLocPair:restLoc) =
  if columnDbPair == columnLocPair
    then getChangedColumnList' restDb restLoc
    else columnLocPair : getChangedColumnList' restDb restLoc
getChangedColumnList' _ _ = []

updateColumnArr :: Table -> ColumnLocalList -> IO ()
updateColumnArr _ [] = return ()
updateColumnArr table (columnPair:rest) = do
  updateColumn table columnPair
  updateColumnArr table rest

updateColumn :: Table -> (T.Text, A.Value) -> IO ()
updateColumn _ _ {-table (column, arrLocValue)-} = undefined--do
--  essenceDbObj <- set =<< setPath ("EssenceDatabase\\" <> table <> ".json")
--  let arrDbValue = getValue ["TABLE",T.pack table,T.pack column] essenceDbObj
--  let arrLocList = toStrArr arrLocValue
--  let arrDbList = toStrArr arrDbValue
--  if isRedundantColumn arrDb arrLoc
--    then dropColumn essence $ ColumnDbList \\ ColumnLocList
--    else infoIO "No need delete Column"
--  if isNewColumn arrDb arrLoc
--    then createColumn essence $ ColumnLocList \\ ColumnDbList
--    else infoIO "No need add Column"
--  changedColumnList <- getChangedColumnList essence
--  if null changedColumnList
--    then infoIO "No need change value"
--    else updateColumnArr table changedColumnList
--updateColumn _ (column, _) =
--  infoIO $ "Column " <> column <> " must contain an array in EssenceLocal/.json"

dropColumns, createColumns :: Table -> ColumnLocalList -> IO ()
dropColumns table columnList = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      let dropQueries = alterTableDrop table columnList
      let getColumns = fst . unzip
      result <- tryRunIO $ HDBC.runRaw conn dropQueries
      case result of
        1 -> do
          deleteColumnJson table $ getColumns columnList
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $
            "Table: " <>
            table <>
            ", Columns: " <>
            (intercalate "," . map T.unpack . getColumns) columnList <>
            " are deleted"
        _ -> HDBC.disconnect conn >> return ()

createColumns table columnList = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      let addQueries = alterTableAdd table columnList
      result <- tryRunIO $ HDBC.runRaw conn addQueries
      case result of
        1 -> do
          replaceColumnJson table $ HM.fromList columnList
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $
            "Table: " <>
            table <>
            ", Columns: " <>
            (intercalate "," . map T.unpack . fst . unzip) columnList <>
            " are created"
        _ -> HDBC.disconnect conn >> return ()

alterTableDrop, alterTableAdd :: Table -> ColumnLocalList -> String
alterTableDrop _ [] = ""
alterTableDrop table ((column, _):rest) =
  "ALTER TABLE " <>
  table <>
  " DROP IF EXISTS " <> T.unpack column <> ";" <> alterTableDrop table rest

alterTableAdd _ [] = ""
alterTableAdd table ((column, value):rest) =
  "ALTER TABLE " <>
  table <>
  " ADD IF NOT EXISTS " <>
  T.unpack column <>
  " " <> intercalate " " (toStrArr value) <> ";" <> alterTableAdd table rest

replaceColumnJson :: Table -> ColumnLocalObj -> IO ()
replaceColumnJson table columnLocObj = do
  (columnDbObj, encodeFileFunc) <- getColumnDbObj table
  let columnNewObj = HM.union columnLocObj columnDbObj
  encodeFileFunc columnNewObj

deleteColumnJson :: Table -> [ColumnLocal] -> IO ()
deleteColumnJson table columnLoc = do
  (columnDbObj, encodeFileFunc) <- getColumnDbObj table
  let columnNewObj = deleteFields columnDbObj columnLoc
  encodeFileFunc columnNewObj

getColumnDbObj :: Table -> IO (ColumnDatabaseObj, A.Object -> IO ())
getColumnDbObj table = do
  path <- setPath $ "EssenceDatabase\\" <> table <> ".json"
  essenceDbObj <- trySetIO $ set path
  let fields = ["TABLE", T.pack table]
  let columnDbObj = fromObj $ getValue fields essenceDbObj
  return (columnDbObj, \x -> A.encodeFile path $ toObj x fields)

toColumnList :: Table -> TableObj -> ColumnList
toColumnList table = HM.toList . fromObj . getValue ["TABLE", T.pack table]

getColumnArr :: Table -> TableObj -> ColumnArr
getColumnArr table = HM.keys . fromObj . getValue ["TABLE", T.pack table]
