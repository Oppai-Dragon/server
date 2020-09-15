module Database.Update
  ( updateDatabase
  ) where

import Config
import Data.Base
import Data.Value
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

type ColumnLocalList = [(T.Text, A.Value)]

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

isRedundantTables, isNewTables ::
     [EssenceApi] -> [EssenceDatabase] -> [EssenceLocal] -> Bool
isRedundantTables apiArr dbArr locArr =
  null (apiArr \\ locArr) && (not . null) (dbArr \\ locArr)

isNewTables apiArr dbArr locArr =
  null (apiArr \\ locArr) && (not . null) (locArr \\ dbArr)

getChangedTables :: [EssenceDatabase] -> [EssenceLocal] -> IO [EssenceLocal]
getChangedTables (essenceDb:restDb) (essenceLoc:restLoc) = do
  essenceDbObj <- set =<< setPath ("EssenceDatabase\\" <> essenceDb)
  essenceLocObj <- set =<< setPath ("EssenceLocal\\" <> essenceLoc)
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
  essenceLocObj <- set =<< setPath ("EssenceLocal\\" <> essence)
  essenceDbObj <- set =<< setPath ("EssenceDatabase\\" <> essence)
  let toColumnsObj obj =
        case getValue ["TABLE", T.pack essence] obj of
          A.Object x -> x
          _ -> HM.empty
  let columnsDbObj = toColumnsObj essenceDbObj
  let columnsDbList = HM.toList columnsDbObj
  let columnsLocObj = toColumnsObj essenceLocObj
  let columnsLocList = HM.toList columnsLocObj
  if isRedundantColumns columnsDbList columnsLocList
    then dropColumns essence $ columnsDbList \\ columnsLocList
    else infoIO "No need delete columns"
  if isNewColumns columnsDbList columnsLocList
    then createColumns essence $ columnsLocList \\ columnsDbList
    else infoIO "No need add columns"
  --changedColumns <- getChangedColumns (sort columnsDb) (sort columnsLoc)
  --if null changedColumns
  --  then infoIO "No need change columns"
  --  else updateTableArr changedColumns
  return ()

isRedundantColumns, isNewColumns ::
     ColumnDatabaseList -> ColumnLocalList -> Bool
isRedundantColumns = ((not . null) .) . (\\)

isNewColumns = ((not . null) .) . flip (\\)

dropColumns, createColumns :: Table -> ColumnLocalList -> IO ()
dropColumns table columnList = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      let dropQueries = alterTableDrop table columnList
      result <- tryRunIO $ HDBC.runRaw conn dropQueries
      case result of
        1 -> do
          replaceEssenceJson [table]
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $
            "Columns: " <>
            (intercalate "," . map T.unpack . fst . unzip) columnList <>
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
          replaceEssenceJson [table]
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $
            "Columns: " <>
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
  " ADD IF NOT EXIST " <>
  T.unpack column <>
  intercalate " " (toStrArr value) <> ";" <> alterTableAdd table rest
