module Data.Request.Control
    ( isPathRequestCorrect
    , ifEveryoneUpdate
    , ifGetUpdate
    , parseRequest
    , isRequestCorrect
    , getAccessArr
    , accessCollector
    ) where

import           Config
import           Data.Base
import           Data.Essence
import           Data.Essence.Methods
import           Data.Empty
import           Data.MyValue
import           Data.Required.Methods                          (getRequiredFields)
import           Data.Request
import           Data.Request.Access
import           Data.Request.Access.Methods                    (isAccess)
import           Data.Request.Method.Methods                    (isMethodCorrect)
import           Data.Request.Params.Methods
import           Data.SQL
import           Data.SQL.Actions

import qualified Data.Text                              as T
import qualified Data.HashMap.Strict                    as HM
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BS8
import           Data.ByteString.Builder.Internal               (byteStringCopy)

import           Data.Monoid

import           Database.HDBC                          as HDBC
import qualified Database.HDBC.PostgreSQL               as PSQL

import           Network.Wai

import           Network.HTTP.Types

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.CPS
import           Control.Monad.Trans.Class          (lift)

import           System.IO.Unsafe                               (unsafePerformIO)

type Name = T.Text
type EssenceName = Name
type Action = T.Text
type QueryMBS = [(BS.ByteString,Maybe BS.ByteString)]

notFound :: Response
notFound = responseBuilder status404 [] "Not found"

isPathRequestCorrect :: Request -> Api -> Bool
isPathRequestCorrect req api
    | length (pathInfo req) <= 1 = False
    | otherwise                  =
        let
            [essence,action] = pathInfo req
            essences = getEssences api
            apiActions = getApiActions api
        in case findText essence essences of
            Just _  -> case findText action apiActions of
                Just _  -> True
                Nothing -> False
            Nothing -> False

ifEveryoneUpdate :: Essence DB -> Access -> Essence DB
ifEveryoneUpdate essenceDB access = if access > Everyone
    then EssenceDB (nameOf essenceDB) (actionOf essenceDB)
        $ HM.insert "access_key"
        (Description (MyString empty) (Just $ NOT NULL) Nothing Nothing)
        (hashMapOf essenceDB)
    else essenceDB

ifGetUpdate :: Essence DB -> Essence DB
ifGetUpdate essenceDB = if (actionOf essenceDB) == "get"
    then EssenceDB (nameOf essenceDB) (actionOf essenceDB)
        $ HM.insert "page"
        (Description (MyInteger empty) Nothing Nothing Nothing)
        (hashMapOf essenceDB)
    else essenceDB

parseRequest :: Request -> IO (EssenceName,Action,QueryMBS,Method)
parseRequest req = do
    let pathReq = pathInfo req
    let essence = head pathReq
    let action = head $ tail pathReq
    queryMBS <- getQueryString req
    let method = requestMethod req
    return (essence,action,queryMBS,method)

isRequestCorrect :: Request -> IO (Bool, Response, Query, Config)
isRequestCorrect req = do
    api <- setApi
    (essence',action,queryMBS,method) <- parseRequest req
    let essence = if action == "publish" then "news" else essence'
    config <- chooseConfig essence
    let essenceDB = getEssenceDB essence action config api
    let essenceFields = getEssenceFields essenceDB api
    let listOfPairs = withoutEmpty $ parseFieldValue essenceFields queryMBS
    let paramsMsg =
            byteStringCopy
            . BS8.pack
            . show
            $ getRequiredFields essenceDB api
    accessArr <- getAccessArr queryMBS
    isConstraintsCorrect <- runReaderT (execWriterT $ isConstraintCorrect essenceDB listOfPairs) config
    let checkingList =
            [isPathRequestCorrect req api
            ,isMethodCorrect method action api
            ,isAccess essence action accessArr api
            ,isRequiredParams essenceDB queryMBS api
            ,getAll $ isTypeParamsCorrect essenceDB listOfPairs
            ,getAll isConstraintsCorrect]
    let elseThenList =
            [(False, notFound)
            ,(False, responseBuilder status400 [] "Incorrect request method")
            ,(False, notFound)
            ,(False, responseBuilder status400 [] paramsMsg)
            ,(False, responseBuilder status400 [] "Incorrect type of params")
            ,(False, responseBuilder status400 [] "Bad values")
            ,(True, notFound)]
    let (x1,x2) = ifElseThen checkingList elseThenList
    pure (x1,x2,queryMBS,config)

getAccessArr :: QueryMBS -> IO [Access]
getAccessArr queryMBS = case lookup "access_key" queryMBS of
    Just (Just accessKeyBS) ->
        ([Everyone] <>) <$> accessCollector accessKeyBS
    _                       -> pure [Everyone]

accessCollector :: BS.ByteString -> IO [Access]
accessCollector accessKeyBS = do
    config <- setConfig
    let accessKeyStr = parseValue $ fromBS accessKeyBS
    let uriDB = getUri config
    conn <- PSQL.connectPostgreSQL uriDB
    let userQuery = "SELECT id,is_admin FROM person WHERE access_key=" <> accessKeyStr <> ";"
    sqlValuesArr <- quickQuery' conn userQuery []
    HDBC.disconnect conn
    let checkIsAdmin bool = if bool then [Person,Admin] else [Person]
    let authorCollect userId = do
            conn <- PSQL.connectPostgreSQL uriDB
            let authorQuery = showSql $ Get "author" [Where ("person_id", MyInteger userId)]
            sqlValuesArr <- quickQuery' conn authorQuery []
            HDBC.disconnect conn
            if null sqlValuesArr
                then pure []
                else pure [Author]
    case sqlValuesArr of
        [SqlInteger id,SqlBool isAdmin]:rest ->
            (checkIsAdmin isAdmin <>) <$> authorCollect id
        _                                    ->
            pure [Everyone]