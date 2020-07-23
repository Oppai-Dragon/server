module Data.Request.Control
    ( isRequestCorrect
    ) where

import           Config
import           Data.Base
import           Data.Essence
import qualified Data.Essence.Builder                   as DEB
import           Data.Essence.Methods
import           Data.MyValue                                   (bsToStr)
import           Data.Required.Methods                          (getRequiredFields)
import           Data.Request.Access
import           Data.Request.Access.Methods                    (isAccess)
import           Data.Request.Method.Methods                    (isMethodCorrect)
import           Data.Request.Params.Methods                    (isRightParams,isTypeParamsCorrect)
import           Data.SQL                                       (get)

import qualified Data.Text                              as T
import qualified Data.HashMap.Strict                    as HM
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BS8
import           Data.ByteString.Builder.Internal               (byteStringCopy)

import           Database.HDBC                          as HDBC
import qualified Database.HDBC.PostgreSQL               as PSQL

import           Network.Wai

import           Network.HTTP.Types

import           System.IO.Unsafe                               (unsafePerformIO)

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
        in case find essence essence of
            Just _  -> case find action apiActions of
                Just _  -> True
                Nothing -> False
            Nothing -> False

isRequestCorrect :: Request -> Config -> (Bool, Response)
isRequestCorrect req conf = do
    let pathReq = pathInfo req
    let essence = head pathReq
    let action = head $ tail pathReq
    let queryBS = queryString req
    let method = requestMethod req
    let api = unsafePerformIO setApi
    let access = getAccess essence action api
    let essenceDB'' = getEssenceDB essence action conf
    let essenceDB' = if access > Everyone
            then DEB.insert "access_key"
                (Description StrV (Just $ NOT NULL) Nothing Nothing)
                essenceDB''
            else essenceDB''
    let essenceDB = if action == "get"
            then DEB.insert "page"
                (Description IntV Nothing Nothing Nothing)
                essenceDB'
            else essenceDB'
    let essenceFields = map T.unpack $ getEssenceFields essence conf
    let listOfPairs = parseFieldValue essenceFields queryBS
    let paramsMsg =
            byteStringCopy
            $ BS8.pack
            $ show
            $ getRequiredFields essenceDB
    let accessArr =
            case lookup "access_key" queryBS of
                Just (Just accessKeyBS) ->
                    ([Everyone] <>) <$> accessCollector accessKeyBS conf
                _                       -> pure [Everyone]
    let accessArr' = unsafePerformIO accessArr
    let checkingList =
            [isMethodCorrect method action conf
            ,isPathRequestCorrect req api
            ,isRightParams essenceDB queryBS
            ,and $ isTypeParamsCorrect essenceDB listOfPairs
            ,isAccess essence action accessArr' conf
            ]
    let elseThenList =
            [(False, responseBuilder status400 [] "Incorrect request method")
            ,(False, notFound)
            ,(False, responseBuilder status400 [] paramsMsg)
            ,(False, responseBuilder status400 [] "Incorrect type of params")
            ,(False, notFound)
            ,(True, undefined)]
    ifElseThen checkingList elseThenList

accessCollector :: BS.ByteString -> Config -> IO [Access]
accessCollector accessKeyBS conf = do
    let accessKeyStr = bsToStr accessKeyBS
    let uriDB = getUriDB conf
    conn <- PSQL.connectPostgreSQL uriDB
    let userQuery = get "users" "id,is_admin" ("access_key='" <> accessKeyStr <> "'")
    sqlValuesArr <- quickQuery' conn userQuery []
    HDBC.disconnect conn
    let checkIsAdmin bool = if bool then [User,Admin] else [User]
    let authorCollect userId = do
            let userIdStr = show userId
            conn <- PSQL.connectPostgreSQL uriDB
            let authorQuery = get "author" "*" ("user_id=" <> userIdStr)
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