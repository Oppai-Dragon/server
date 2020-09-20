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
  , parseAllQueries
  , iterateObj
  ) where

import Config
import Config.Exception
import Data.Base
import Database.Exception
import Log

import Control.Monad
import qualified Data.Aeson as A
import Data.Function
import Data.Functor.Identity
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
  return [obj | obj <- [parseOnlyTable o | o <- objList], not $ null obj]

parseOnlyTable :: A.Object -> A.Object
parseOnlyTable obj =
  case getValue ["TABLE"] obj of
    A.Object o -> o
    _ -> HM.empty

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

iterateObj :: A.Object -> Writer String ()
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
