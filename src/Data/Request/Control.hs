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
import           Data.Request.Access
import           Data.Request.Access.Methods                    (isAccess)
import           Data.Request.Method.Methods                    (isMethodCorrect)
import           Data.Request.Params.Methods
import qualified Data.SQL                               as SQL

import qualified Data.Text                              as T
import qualified Data.HashMap.Strict                    as HM
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BS8
import           Data.ByteString.Builder.Internal               (byteStringCopy)

import           Database.HDBC                          as HDBC
import qualified Database.HDBC.PostgreSQL               as PSQL

import           Network.Wai

import           Network.HTTP.Types

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
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
        (fieldsOf essenceDB)
    else essenceDB

ifGetUpdate :: Essence DB -> Essence DB
ifGetUpdate essenceDB = if (actionOf essenceDB) == "get"
    then EssenceDB (nameOf essenceDB) (actionOf essenceDB)
        $ HM.insert "page"
        (Description (MyInteger empty) Nothing Nothing Nothing)
        (fieldsOf essenceDB)
    else essenceDB

parseRequest :: Request -> (EssenceName,Action,QueryMBS,Method)
parseRequest req =
    let
        pathReq = pathInfo req
        essence = head pathReq
        action = head $ tail pathReq
        queryMBS = queryString req
        method = requestMethod req
    in (essence,action,queryMBS,method)

isRequestCorrect :: Request -> ReaderT Config IO (Bool, Response)
isRequestCorrect req = do
    config <- ask
    api <- lift setApi
    let (essence,action,queryMBS,method) = parseRequest req
    let access = getAccess essence action api
    let essenceDB'' = getEssenceDB essence action config
    let essenceDB'  = ifEveryoneUpdate essenceDB'' access
    let essenceDB   = ifGetUpdate essenceDB'
    let essenceFields = map T.unpack $ getEssenceFields essence config
    let listOfPairs = withoutEmpty $ parseFieldValue essenceFields queryMBS
    let paramsMsg =
            byteStringCopy
            . BS8.pack
            . show
            $ getRequiredFields essenceDB
    accessArr <- lift $ getAccessArr queryMBS
    let checkingList =
            [isPathRequestCorrect req api
            ,isMethodCorrect method action api
            ,isRequiredParams essenceDB queryMBS
            ,and $ isTypeParamsCorrect essenceDB listOfPairs
            ,isAccess essence action accessArr api]
    let elseThenList =
            [(False, notFound)
            ,(False, responseBuilder status400 [] "Incorrect request method")
            ,(False, responseBuilder status400 [] paramsMsg)
            ,(False, responseBuilder status400 [] "Incorrect type of params")
            ,(False, notFound)
            ,(True, notFound)]
    pure $ ifElseThen checkingList elseThenList

getAccessArr :: QueryMBS -> IO [Access]
getAccessArr queryMBS = case lookup "access_key" queryMBS of
    Just (Just accessKeyBS) ->
        ([Everyone] <>) <$> accessCollector accessKeyBS
    _                       -> pure [Everyone]

accessCollector :: BS.ByteString -> IO [Access]
accessCollector accessKeyBS = do
    config <- setConfig
    let accessKeyStr = fromBS accessKeyBS
    let uriDB = getUri config
    conn <- PSQL.connectPostgreSQL uriDB
    let userQuery = SQL.get "person" "id,is_admin" ("access_key=" <> parseEmpty accessKeyStr)
    sqlValuesArr <- quickQuery' conn userQuery []
    HDBC.disconnect conn
    let checkIsAdmin bool = if bool then [User,Admin] else [User]
    let authorCollect userId = do
            let userIdStr = show userId
            conn <- PSQL.connectPostgreSQL uriDB
            let authorQuery = SQL.get "author" "*" ("user_id=" <> userIdStr)
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