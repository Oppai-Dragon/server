{-# LANGUAGE FlexibleContexts #-}

module Setup
  ( setup
  , resetup
  , getConnection
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
import qualified Data.Text as T
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL
import Debug.Trace
import qualified System.Directory as Dir

extensionQuery :: String
extensionQuery = "CREATE EXTENSION IF NOT EXISTS pgcrypto;"

-- For deleting all tables
getDropTablesQuery :: [T.Text] -> String
getDropTablesQuery essences =
  T.unpack $ "DROP TABLE " <> T.intercalate ", " essences `T.snoc` ';'

setup :: IO ()
setup = createTables >> buildConfigJson

resetup :: IO ()
resetup = dropTables >> setup

getConnection :: IO PSQL.Connection
getConnection = do
  uriDB <- getUriDB <$> setPsql
  conn <- PSQL.connectPostgreSQL uriDB
  HDBC.runRaw conn setEng
  return conn

dropTables :: IO ()
dropTables = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      api <- setApi
      let essences = getEssences api
      let dropQuery = getDropTablesQuery essences
      deleteEssenceJson $ map T.unpack essences
      _ <- tryRunIO $ HDBC.run conn dropQuery []
      HDBC.commit conn
      HDBC.disconnect conn
      traceIO "Database for server is rebuilded, all tables are empty"

buildConfigJson :: IO ()
buildConfigJson = do
  path <- setConfigPath
  psql <- setPsql
  let uriObj = HM.singleton "uriDB" . A.String . T.pack $ getUriDB psql
  objEssenceList <- collectEssenceJson
  let jsonObj = HM.unions objEssenceList
  let json = A.Object $ HM.unions [jsonObj, uriObj]
  A.encodeFile path json

createTables :: IO ()
createTables = do
  maybeConn <- tryConnectIO getConnection
  case maybeConn of
    Nothing -> return ()
    Just conn -> do
      sqlQueries <- getAllQueris
      _ <- HDBC.run conn extensionQuery []
      ioStack (flip ((tryRunIO .) . HDBC.run conn) []) sqlQueries
      tables <- HDBC.getTables conn
      essences <- getEssences <$> setApi
      if tables == map T.unpack essences
        then HDBC.commit conn >> replaceEssenceJson tables >>
             HDBC.disconnect conn >>
             traceIO "Success"
        else HDBC.disconnect conn >> traceIO "Failed"

replaceEssenceJson :: [String] -> IO ()
replaceEssenceJson [] = return ()
replaceEssenceJson (essence:rest) = do
  essencePath <- setPath $ "EssenceLocal\\" <> essence <> ".json"
  obj <- trySetIO $ set essencePath
  path <- setPath $ "EssenceDatabase\\" <> essence <> ".json"
  A.encodeFile path $ A.Object obj
  replaceEssenceJson rest

deleteEssenceJson :: [String] -> IO ()
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

getEssenceDBObjectArr :: [String] -> IO [A.Object]
getEssenceDBObjectArr [] = return []
getEssenceDBObjectArr (essence:rest) =
  (:) <$> getEssenceDBObject essence <*> getEssenceDBObjectArr rest

getEssenceLocalObjectArr :: [String] -> IO [A.Object]
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

getAllQueris :: IO [String]
getAllQueris = do
  api <- setApi
  let essences = getEssences api
  createObjList <- getEssenceLocalObjectArr $ map T.unpack essences
  return ["CREATE " <> getCreateQuery obj | obj <- createObjList]

getCreateQuery :: A.Object -> String
getCreateQuery obj = parseAllQueries . runIdentity . execWApp $ iterateObj obj

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
