module Data.Request.Control
  ( isPathRequestCorrect
  , ifEveryoneUpdate
  , ifGetUpdate
  , parseRequest
  , isRequestCorrect
  , getAccessArr
  , accessCollector
  ) where

import Config
import Data.Base
import Data.Empty
import Data.Essence
import Data.Essence.Column hiding (Action(..))
import Data.Essence.Methods
import Data.MyValue
import Data.Request
import Data.Request.Access
import Data.Request.Access.IsRight
import Data.Request.Method.IsRight
import Data.Request.Params.Methods
import Data.SQL
import Data.SQL.ShowSql
import Log

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai

type Name = T.Text

type EssenceName = Name

type Action = T.Text

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]

isPathRequestCorrect :: Wai.Request -> Api -> Bool
isPathRequestCorrect req api
  | length (Wai.pathInfo req) <= 1 = False
  | otherwise =
    let [essence, action] = Wai.pathInfo req
        essences = getEssences api
        apiActions = getApiActions api
     in case findText essence essences of
          Just _ ->
            case findText action apiActions of
              Just _ -> True
              Nothing -> False
          Nothing -> False

ifEveryoneUpdate :: Essence Column -> Access -> Essence Column
ifEveryoneUpdate essenceColumn@(EssenceColumn { eColName = name
                                              , eColAction = action
                                              , eColHashMap = hashMap
                                              }) access =
  if access > Everyone
    then EssenceColumn name action $
         HM.insert
           "access_key"
           defaultColumn {cValueType = TEXT, cNULL = Just $ NOT NULL}
           hashMap
    else essenceColumn

ifGetUpdate :: Essence Column -> Essence Column
ifGetUpdate essenceColumn@(EssenceColumn { eColName = name
                                         , eColAction = action
                                         , eColHashMap = hashMap
                                         }) =
  case action of
    "get" ->
      EssenceColumn name action $
      HM.insert "page" defaultColumn {cValueType = INT} hashMap
    _ -> essenceColumn

parseRequest ::
     Wai.Request -> IO (EssenceName, Action, QueryMBS, HTTPTypes.Method)
parseRequest req = do
  let pathReq = Wai.pathInfo req
  let essence = head pathReq
  let action = head $ tail pathReq
  queryMBS <- getQueryString req
  let method = Wai.requestMethod req
  return (essence, action, queryMBS, method)

isRequestCorrect ::
     Wai.Request -> IO (Bool, Wai.Response, QueryMBS, Config.Handle)
isRequestCorrect req = do
  (essence', action, queryMBS, method) <- parseRequest req
  let essence =
        if action == "publish"
          then "news"
          else essence'
  handle@(Config.Handle config api _ logHandle) <- Config.new
  let essenceColumn = getEssenceColumn essence action config api
  let essenceFields = getEssenceFields essenceColumn api
  let listOfPairs = withoutEmpty $ parseFieldValue essenceFields queryMBS
  let paramsMsg = fromString . show $ getRequiredFields essenceColumn api
  accessArr <- getAccessArr queryMBS
  isConstraintsCorrect <-
    runUnderApp
      (execWApp $ isConstraintCorrect essenceColumn listOfPairs)
      handle
  let checkingList =
        [ isPathRequestCorrect req api
        , isMethodCorrect method action api
        , isAccess essence action accessArr api
        , isRequiredParams essenceColumn queryMBS api
        , getAll $ isTypeParamsCorrect essenceColumn listOfPairs
        , getAll isConstraintsCorrect
        ]
  let elseThenList =
        [ (All False, notFoundWith "Incorrect request path")
        , (All False, notFoundWith "Incorrect request method")
        , (All False, notFound)
        , (All False, notFoundWith paramsMsg)
        , (All False, notFoundWith "Incorrect type of params")
        , (All False, notFoundWith "Bad values")
        , (All True, notFound)
        ]
  debugM logHandle $ "Accesses of request " <> show accessArr
  let (All x1, x2) = ifElseThen checkingList elseThenList
  pure (x1, x2, queryMBS, handle)

getAccessArr :: QueryMBS -> IO [Access]
getAccessArr queryMBS =
  case lookup "access_key" queryMBS of
    Just (Just accessKeyBS) -> ([Everyone] <>) <$> accessCollector accessKeyBS
    _ -> pure [Everyone]

accessCollector :: BS.ByteString -> IO [Access]
accessCollector accessKeyBS = do
  psql <- setPsql
  let accessKeyStr = parseValue $ fromBS accessKeyBS
  let uriDB = getUriDB psql
  conn <- PSQL.connectPostgreSQL uriDB
  HDBC.runRaw conn setEng
  let userQuery =
        "SELECT id,is_admin FROM person WHERE access_key=" <>
        accessKeyStr <> ";"
  personValuesArr <- HDBC.quickQuery' conn userQuery []
  let checkIsAdmin bool =
        if bool
          then [Person, Admin]
          else [Person]
  let authorCollect userId = do
        let authorQuery =
              showSql $ Get "author" [Where ("person_id", MyInteger userId)]
        authorValuesArr <- HDBC.quickQuery' conn authorQuery []
        HDBC.disconnect conn
        if null authorValuesArr
          then pure []
          else pure [Author]
  case personValuesArr of
    [HDBC.SqlInteger num, HDBC.SqlBool isAdmin]:_ ->
      (checkIsAdmin isAdmin <>) <$> authorCollect num
    _ -> HDBC.disconnect conn >> pure [Everyone]
