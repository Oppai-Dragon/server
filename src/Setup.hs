{-# LANGUAGE FlexibleContexts #-}

module Setup
  ( setup
  , resetup
  , buildConfigJson
  , createTables
  , collectEssenceJson
  , getEssenceObjects
  , getAllQueris
  , getCreateQuery
  , parseAllQueries
  , iterateObj
  ) where

import Config
import Data.Base
import Data.Value

import qualified Data.Aeson as A
import Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import Data.List as L
import qualified Data.Text as T
import Debug.Trace

import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

import System.IO.Unsafe (unsafePerformIO)

-- For changing settings in SQL Shell (psql)
-- SET lc_messages = 'en_US.UTF8'; SET client_encoding = 'UTF8';
-- For deleting all tables
dropTablesQuery :: String
dropTablesQuery =
  "DROP TABLE person, author, category, tag, news, draft, comment;"

setup :: IO ()
setup = do
  buildConfigJson
  createTables

resetup :: IO ()
resetup = do
  dropTables
  setup

getConnection :: IO PSQL.Connection
getConnection = do
  psql <- setPsql
  let uriDB = getUriDB psql
  PSQL.connectPostgreSQL uriDB

dropTables :: IO ()
dropTables = do
  conn <- getConnection
  _ <- HDBC.run conn dropTablesQuery []
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
  conn <- getConnection
  sqlQueries <- getAllQueris
  ioStack (flip (HDBC.run conn) []) sqlQueries
  HDBC.commit conn
  HDBC.disconnect conn
  traceIO "Success"

collectEssenceJson :: IO [A.Object]
collectEssenceJson = do
  objList <- getEssenceObjects
  let parseOnlyTable obj =
        case getValue ["TABLE"] obj of
          A.Object o -> o
          _ -> HM.empty
  let result = [obj | obj <- [parseOnlyTable o | o <- objList], not $ null obj]
  return result

getEssenceObjects :: IO [A.Object]
getEssenceObjects = do
  api <- setApi
  let essences = getEssences api
  let createStrList = "extension" : map T.unpack essences
  let createObjList =
        map
          (\x -> unsafePerformIO $ set =<< setPath ("\\Setup\\" <> x <> ".json"))
          createStrList
  return createObjList

getAllQueris :: IO [String]
getAllQueris = do
  createObjList <- getEssenceObjects
  let result =
        (\x -> L.delete ')' (head x) : tailCase x)
          ["CREATE " <> getCreateQuery obj | obj <- createObjList]
  return result

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
