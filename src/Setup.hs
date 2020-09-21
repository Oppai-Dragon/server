{-# LANGUAGE FlexibleContexts #-}

module Setup
  ( setup
  , resetup
  , getConnection
  , dropTables
  , buildConfigJson
  , createTables
  , replaceTableJson
  , collectEssenceJson
  , getEssenceDescriptionObjectArr
  , getEssenceLocalObjectArr
  , getEssenceDescriptionObject
  , getEssenceLocalObject
  , getAllQueris
  , getCreateQuery
  ) where

import Config
import Config.Exception
import Data.Base
import Data.Essence
import Data.SQL.ShowSql
import Database.Exception
import Data.MyValue
import Log

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL
import qualified System.Directory as Dir

type EssenceArr = [String]

extensionQuery :: String
extensionQuery = "CREATE EXTENSION IF NOT EXISTS pgcrypto;"

-- For deleting all tables
getDropTablesQuery :: EssenceArr -> String
getDropTablesQuery essences = "DROP TABLE " <> intercalate ", " essences <> ";"

setup :: IO ()
setup = do
  essences <- map T.unpack . getEssences <$> setApi
  createTables essences
  buildConfigJson
  createFirstAdmin

resetup :: IO ()
resetup = do
  essences <- map T.unpack . getEssences <$> setApi
  dropTables essences
  setup

createFirstAdmin :: IO ()
createFirstAdmin = do
  let adminEssenceColumn =
        EssenceColumn "person" "create" $
        HM.fromList
          [ ("first_name", MyString "First")
          , ("last_name", MyString "Admin")
          , ("access_key", MyString "12345678-1234-1234-1234-123456789abc")
          ]
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      let createQuery = showSql adminEssenceColumn
      result <- tryRunIO $ HDBC.run conn createQuery []
      case result of
        Success -> do
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $ "First Admin is created "
        Fail -> HDBC.disconnect conn >> return ()

getConnection :: IO PSQL.Connection
getConnection = do
  uriDB <- getUriDB <$> setPsql
  conn <- PSQL.connectPostgreSQL uriDB
  HDBC.runRaw conn setEng
  return conn

dropTables :: EssenceArr -> IO ()
dropTables essences = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      let dropQuery = getDropTablesQuery essences
      result <- tryRunIO $ HDBC.run conn dropQuery []
      case result of
        Success -> do
          deleteTableJson essences
          HDBC.commit conn
          HDBC.disconnect conn
          infoIO $ "Tables: " <> intercalate "," essences <> " are deleted"
        Fail -> HDBC.disconnect conn >> return ()

buildConfigJson :: IO ()
buildConfigJson = do
  path <- setConfigPath
  psql <- setPsql
  let uriObj = HM.singleton "uriDB" . A.String . T.pack $ getUriDB psql
  objEssenceList <- collectEssenceJson
  let jsonObj = HM.unions objEssenceList
  let json = A.Object $ HM.unions [jsonObj, uriObj]
  A.encodeFile path json

createTables :: EssenceArr -> IO ()
createTables essences = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      sqlQueries <- getAllQueris essences
      _ <- HDBC.run conn extensionQuery []
      ioStack (flip ((tryRunIO .) . HDBC.run conn) []) sqlQueries
      tables <- HDBC.getTables conn
      essencesApi <- map T.unpack . getEssences <$> setApi
      if tables == essencesApi
        then do
          HDBC.commit conn
          replaceTableJson tables
          HDBC.disconnect conn
          infoIO $ "Tables: " <> intercalate "," essences <> " are created"
        else do
          HDBC.disconnect conn
          infoIO $ "Tables: " <> intercalate "," essences <> " aren't created"

replaceTableJson :: EssenceArr -> IO ()
replaceTableJson [] = return ()
replaceTableJson (essence:rest) = do
  essencePath <- setPath $ "EssenceLocal\\" <> essence <> ".json"
  obj <- trySetIO $ set essencePath
  path <- setPath $ "EssenceDatabase\\" <> essence <> ".json"
  A.encodeFile path $ A.Object obj
  replaceTableJson rest

deleteTableJson :: EssenceArr -> IO ()
deleteTableJson [] = return ()
deleteTableJson (essence:rest) = do
  path <- setPath $ "EssenceDatabase\\" <> essence <> ".json"
  isExist <- Dir.doesFileExist path
  when isExist $ Dir.removeFile path
  deleteTableJson rest

collectEssenceJson :: IO [A.Object]
collectEssenceJson = do
  api <- setApi
  let essences = getEssences api
  nowConfig <- set =<< setPath "Config.json"
  objList <-
    map T.unpack essences &
    if HM.null nowConfig
      then getEssenceLocalObjectArr
      else getEssenceDescriptionObjectArr
  return [obj | obj <- objList, not $ null obj]

getEssenceDescriptionObjectArr :: EssenceArr -> IO [A.Object]
getEssenceDescriptionObjectArr [] = return []
getEssenceDescriptionObjectArr (essence:rest) =
  (:) <$> getEssenceDescriptionObject essence <*>
  getEssenceDescriptionObjectArr rest

getEssenceLocalObjectArr :: EssenceArr -> IO [A.Object]
getEssenceLocalObjectArr [] = return []
getEssenceLocalObjectArr (essence:rest) =
  (:) <$> getEssenceLocalObject essence <*> getEssenceLocalObjectArr rest

getEssenceDescriptionObject :: String -> IO A.Object
getEssenceDescriptionObject essence = do
  path <- setPath ("EssenceDatabase\\" <> essence <> ".json")
  obj <- trySetIO $ set path
  return obj

getEssenceLocalObject :: String -> IO A.Object
getEssenceLocalObject essence = do
  path <- setPath ("EssenceLocal\\" <> essence <> ".json")
  obj <- trySetIO $ set path
  return obj

getAllQueris :: EssenceArr -> IO [String]
getAllQueris essences = do
  createObjList <- getEssenceLocalObjectArr essences
  return [getCreateQuery obj | obj <- createObjList]

getCreateQuery :: A.Object -> String
getCreateQuery obj =
  case AT.parseMaybe A.parseJSON (A.Object obj) :: Maybe (Essence Column) of
    Just x -> showSql x
    Nothing -> "Can't parse from json " <> show obj
