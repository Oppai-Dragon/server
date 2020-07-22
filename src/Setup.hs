{-# LANGUAGE FlexibleContexts #-}
module Setup
    ( getCreateQuery
    ) where

import Config
import Data.Base (reverseMap,map2Var,ioStack)
import Data.FromValue (toTextArr)

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V

import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL

import Control.Monad.Trans.Writer.CPS
import           Control.Monad.Trans.Class          (lift)

import qualified Database.HDBC.PostgreSQL as PSQL
import           Database.HDBC

import System.IO.Unsafe                         (unsafePerformIO)
import System.Directory                         (getCurrentDirectory)

-- For changing settings in SQL Shell (psql)
-- SET lc_messages = 'en_US.UTF8'; SET client_encoding = 'UTF8';

-- For deleting all tables
-- DROP TABLE person, author, category, tag, news, draft, comment;

setup :: IO ()
setup = do
    createTables
    buildConfigJson

buildConfigJson :: IO ()
buildConfigJson = do
    path <- setConfigPath
    api <- setApi
    objEssenceList <- collectEssenceJson
    let jsonObj =  HM.unions objEssenceList
    let essences = getEssences api
    let methodFieldsObj = getMethodsFields essences jsonObj
    let json = Object $ methodFieldsObj `HM.union` jsonObj `HM.union`
    encodeFile path json

createTables :: IO ()
createTables = do
    psql <- setPsql
    let uriDB = getUriDB psql
    conn <- PSQL.connectPostgreSQL uriDB
    sqlQueries <- getAllQueris
    ioStack (flip (run conn) []) sqlQueries
    let success = commit conn >> disconnect conn >> print "Success"
    --let fail = rollback conn >> disconnect conn >> print "Fail"
    success

collectEssenceJson :: IO [Object]
collectEssenceJson = do
    objList <- getEssenceObjects
    let parseOnlyTable obj =
            case parseMaybe (.: "TABLE") obj of
                Just (Object o) -> o
                _               -> HM.empty
    let result = [obj | obj <- [parseOnlyTable o | o <- objList], not $ null obj]
    return result

getEssenceObjects :: IO [Object]
getEssenceObjects = do
    api <- setApi
    let essences = getEssences api
    let createStrList = "extension" : map T.unpack essences
    let createObjList =
            map
            (\x -> unsafePerformIO $ set $ setPath $ "\\Setup\\" <> x <> ".json")
            createStrList
    return createObjList

getAllQueris :: IO [String]
getAllQueris = do
    createObjList <- getEssenceObjects
    let result =
            (\x -> L.delete ')' (head x) : tail x)
            [ "CREATE " <> getCreateQuery obj | obj <- createObjList]
    return result

getCreateQuery :: Object -> String
getCreateQuery obj =
    parseAllQueries . execWriter . iterateObj . Object $ obj

parseAllQueries :: String -> String
parseAllQueries str =
    let
        addingSemicolon = flip (<>) ");" . init
        addingLBracket = map2Var
            (\x1 x2 -> if x1 == "TABLE" then x2 <> "(" else x2)
    in addingSemicolon . unwords .
    addingLBracket . words $ str

iterateObj :: Value -> Writer String ()
iterateObj (Object obj') =
    let
        parseObj a obj = parseMaybe (.: a) obj

        iterate []      = tell ""
        iterate (a:arr) = do
            tell $ T.unpack a <> " "
            case parseObj a obj' of
                Just (Object obj) ->
                    (tell . execWriter . iterateObj) (Object obj)
                    >> iterate arr
                Just (Array vector) ->
                    (tell . L.intercalate " " . map T.unpack . toTextArr)
                    (Array vector)
                    >> tell " ,"
                    >> iterate arr
                Just (String text)->
                    tell (T.unpack text)
                    >> tell " ,"
                    >> iterate arr
    in iterate $ HM.keys obj'