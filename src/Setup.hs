{-# LANGUAGE FlexibleContexts #-}

module Setup
  ( setup
  , resetup
  , getConnection
  , dropTables
  , buildConfigJson
  , createTables
  , replaceEssenceJson
  , collectEssenceJson
  , getEssenceDBObjectArr
  , getEssenceLocalObjectArr
  , getEssenceDBObject
  , getEssenceLocalObject
  , getAllQueris
  , getCreateQuery
  , parseAllQueries
  , iterateObj
  ) where

import Config
import Config.Exception
import Data.Base
import Data.Value
import Database.Exception

import Control.Monad
import qualified Data.Aeson as A
import Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL
import Debug.Trace
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

resetup :: IO ()
resetup = do
  essences <- map T.unpack . getEssences <$> setApi
  dropTables essences
  setup

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
      deleteEssenceJson essences
      _ <- tryRunIO $ HDBC.run conn dropQuery []
      HDBC.commit conn
      HDBC.disconnect conn
      traceIO $ "Tables: " <> intercalate "," essences <> " are deleted"

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
          replaceEssenceJson tables
          HDBC.disconnect conn
          traceIO $ "Tables: " <> intercalate "," essences <> " are created"
        else do
          HDBC.disconnect conn
          traceIO $ "Tables: " <> intercalate "," essences <> " aren't created"

replaceEssenceJson :: EssenceArr -> IO ()
replaceEssenceJson [] = return ()
replaceEssenceJson (essence:rest) = do
  essencePath <- setPath $ "EssenceLocal\\" <> essence <> ".json"
  obj <- trySetIO $ set essencePath
  path <- setPath $ "EssenceDatabase\\" <> essence <> ".json"
  A.encodeFile path $ A.Object obj
  replaceEssenceJson rest

deleteEssenceJson :: EssenceArr -> IO ()
deleteEssenceJson [] = return ()
deleteEssenceJson (essence:rest) = do
  path <- setPath $ "EssenceDatabase\\" <> essence <> ".json"
  isExist <- Dir.doesFileExist path
  when isExist $ Dir.removeFile path
  deleteEssenceJson rest

collectEssenceJson :: IO [A.Object]
collectEssenceJson = do
  api <- setApi
  let essences = getEssences api
  objList <- getEssenceDBObjectArr $ map T.unpack essences
  return [obj | obj <- [parseOnlyTable o | o <- objList], not $ null obj]

parseOnlyTable :: A.Object -> A.Object
parseOnlyTable obj =
  case getValue ["TABLE"] obj of
    A.Object o -> o
    _ -> HM.empty

getEssenceDBObjectArr :: EssenceArr -> IO [A.Object]
getEssenceDBObjectArr [] = return []
getEssenceDBObjectArr (essence:rest) =
  (:) <$> getEssenceDBObject essence <*> getEssenceDBObjectArr rest

getEssenceLocalObjectArr :: EssenceArr -> IO [A.Object]
getEssenceLocalObjectArr [] = return []
getEssenceLocalObjectArr (essence:rest) =
  (:) <$> getEssenceLocalObject essence <*> getEssenceLocalObjectArr rest

getEssenceDBObject :: String -> IO A.Object
getEssenceDBObject essence = do
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
  (<>) "CREATE " . parseAllQueries . runIdentity . execWApp $ iterateObj obj

parseAllQueries :: String -> String
parseAllQueries str =
  let addingSemicolon = flip (<>) ");" . init
      addingLBracket =
        map2Var
          (\x1 x2 ->
             case x1 of
               "TABLE" -> x2 <> " ("
               _ -> x2)
   in addingSemicolon . unwords . addingLBracket . words $ str

iterateObj :: A.Object -> W String ()
iterateObj obj =
  let helper [] = tellWApp ""
      helper (a:arr) = do
        tellWApp $ T.unpack a <> " "
        case getValue [a] obj of
          A.Object x ->
            (tellWApp . runIdentity . execWApp . iterateObj) x >> helper arr
          A.Array vector ->
            (tellWApp . unwords . map T.unpack . toTextArr) (A.Array vector) >>
            tellWApp " ," >>
            helper arr
          A.String text ->
            tellWApp (T.unpack text) >> tellWApp " ," >> helper arr
          _ -> helper arr
   in helper $ HM.keys obj
