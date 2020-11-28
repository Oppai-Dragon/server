module Database.Update
  ( updateDatabase
  ) where

import Config
import Config.Exception
import Data.Base
import Data.Essence.Methods
import Data.SQL.AlterTable
import Database.Exception
import Log
import Setup

import Control.Monad
import Control.Monad.Trans.Writer.CPS
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Database.HDBC as HDBC

type Table = String

type EssenceLocal = String

type TableObj = A.Object

type ColumnNameArr = [T.Text]

type ColumnNameLocalArr = [T.Text]

type ColumnNameDatabaseArr = [T.Text]

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
    then void . createTables $ essenceLocalArr1 \\ essenceDatabaseArr1
    else infoIO "No need add tables"
  essenceDatabaseArr2 <- getEssenceArr "EssenceDatabase"
  essenceLocalArr2 <- getEssenceArr "EssenceLocal"
  changedTables <- getChangedTables essenceDatabaseArr2 essenceLocalArr2
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
  essenceDbObj <- set =<< setPath ("configs/EssenceDatabase/" <> essenceDb <> ".json")
  essenceLocObj <- set =<< setPath ("configs/EssenceLocal/" <> essenceLoc <> ".json")
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
  essenceLocObj <- set =<< setPath ("configs/EssenceLocal/" <> essence <> ".json")
  essenceDbObj <- set =<< setPath ("configs/EssenceDatabase/" <> essence <> ".json")
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

isRedundantColumns, isNewColumns ::
     ColumnNameDatabaseArr -> ColumnNameLocalArr -> Bool
isRedundantColumns = ((not . null) .) . (\\)

isNewColumns = ((not . null) .) . flip (\\)

getChangedColumnList :: Table -> IO ColumnLocalList
getChangedColumnList table = do
  essenceDbObj <- set =<< setPath ("configs/EssenceDatabase/" <> table <> ".json")
  essenceLocObj <- set =<< setPath ("configs/EssenceLocal/" <> table <> ".json")
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
updateColumn table (column, columnLocObj) = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      essenceDbObj <- set =<< setPath ("configs/EssenceDatabase/" <> table <> ".json")
      let columnLoc = setColumn columnLocObj
      let columnDbObj = getValue [T.pack table, column] essenceDbObj
      let columnDb = setColumn columnDbObj
      let alterTableQueries =
            execWriter $
            getAlterQueries table (T.unpack column) columnLoc columnDb
      result <- tryRunIO $ HDBC.runRaw conn alterTableQueries
      case result of
        Success -> do
          replaceColumnJson table $ HM.singleton column columnLocObj
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $
            "Table: " <>
            table <> ", Column: " <> T.unpack column <> " was updated"
        Fail -> void $ HDBC.disconnect conn

dropColumns, createColumns :: Table -> ColumnLocalList -> IO ()
dropColumns table columnList = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      let dropQueries = alterTableDrop table columnList
      result <- tryRunIO $ HDBC.runRaw conn dropQueries
      case result of
        Success -> do
          deleteColumnJson table $ map fst columnList
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $
            "Table: " <>
            table <>
            ", Columns: " <>
            (intercalate "," . map (T.unpack . fst)) columnList <>
            " are deleted"
        Fail -> void $ HDBC.disconnect conn

createColumns table columnList = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      let addQueries = alterTableAdd table columnList
      result <- tryRunIO $ HDBC.runRaw conn addQueries
      case result of
        Success -> do
          replaceColumnJson table $ HM.fromList columnList
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $
            "Table: " <>
            table <>
            ", Columns: " <>
            (intercalate "," . map (T.unpack . fst)) columnList <>
            " are created"
        Fail -> void $ HDBC.disconnect conn

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
  " " <> unwords (toStrArr value) <> ";" <> alterTableAdd table rest

replaceColumnJson :: Table -> ColumnLocalObj -> IO ()
replaceColumnJson table columnLocObj = do
  (columnDbObj, encodeFileFunc) <- getColumnDbObj table
  let columnNewObj = HM.union columnLocObj columnDbObj
  encodeFileFunc columnNewObj

deleteColumnJson :: Table -> ColumnNameLocalArr -> IO ()
deleteColumnJson table columnLoc = do
  (columnDbObj, encodeFileFunc) <- getColumnDbObj table
  let columnNewObj = deleteFields columnDbObj columnLoc
  encodeFileFunc columnNewObj

getColumnDbObj :: Table -> IO (ColumnDatabaseObj, A.Object -> IO ())
getColumnDbObj table = do
  path <- setPath $ "configs/EssenceDatabase/" <> table <> ".json"
  essenceDbObj <- trySetIO $ set path
  let fields = [T.pack table]
  let columnDbObj = fromObj $ getValue fields essenceDbObj
  return (columnDbObj, \x -> A.encodeFile path $ toObj x fields)

toColumnList :: Table -> TableObj -> ColumnList
toColumnList table = HM.toList . fromObj . getValue [T.pack table]

getColumnArr :: Table -> TableObj -> ColumnNameArr
getColumnArr table = HM.keys . fromObj . getValue [T.pack table]
