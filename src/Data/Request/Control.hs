module Data.Request.Control
  ( RequestInfo(..)
  , RequestCorrect(..)
  , RequestAnswer(..)
  , isPathRequestCorrect
  , ifNotEveryoneUpdate
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
import Data.Request.Access.Check
import Data.Request.Method.Check
import Data.Request.Params.Check
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

data RequestInfo =
  RequestInfo
    { reqIEssenceName :: EssenceName
    , reqIAction :: Action
    , reqIQueryMBS :: QueryMBS
    , reqIMethod :: HTTPTypes.Method
    }
  deriving (Show,Eq)

data RequestCorrect =
  RequestCorrect
    { reqAnswer :: RequestAnswer
    , reqCQueryMBS :: QueryMBS
    , reqCConfigHandle :: Config.Handle
    }

data RequestAnswer =
  RequestAnswer
    { reqAnswerBool :: Bool
    , reqAnswerResponse :: Wai.Response
    }

isPathRequestCorrect :: Wai.Request -> Api -> Bool
isPathRequestCorrect req api
  | length (Wai.pathInfo req) <= 1 = False
  | otherwise =
    let [essence, action] = Wai.pathInfo req
        essences = getEssences api
        apiActions = getApiActions api
     in all maybeToBool [findText essence essences, findText action apiActions]

ifNotEveryoneUpdate :: Essence Column -> Access -> Essence Column
ifNotEveryoneUpdate essenceColumn@EssenceColumn {eColHashMap = hashMap} access =
  if access > Everyone
    then essenceColumn
           { eColHashMap =
               HM.insert
                 "access_key"
                 defaultColumn {cValueType = UUID, cNULL = Just $ NOT NULL}
                 hashMap
           }
    else essenceColumn

ifGetUpdate :: Essence Column -> Essence Column
ifGetUpdate essenceColumn@EssenceColumn { eColAction = action
                                        , eColHashMap = hashMap
                                        } =
  case action of
    "get" ->
      essenceColumn
        { eColHashMap =
            HM.insert "page" defaultColumn {cValueType = INT} hashMap
        }
    _ -> essenceColumn

parseRequest :: Wai.Request -> IO RequestInfo
parseRequest req = do
  let pathReq = Wai.pathInfo req
  let essence = head pathReq
  let action = head $ tail pathReq
  queryMBS <- getQueryString req
  let method = Wai.requestMethod req
  return
    RequestInfo
      { reqIEssenceName = essence
      , reqIAction = action
      , reqIQueryMBS = queryMBS
      , reqIMethod = method
      }

isRequestCorrect :: Wai.Request -> IO RequestCorrect
isRequestCorrect req = do
  RequestInfo { reqIEssenceName = essence'
              , reqIAction = action
              , reqIQueryMBS = queryMBS
              , reqIMethod = method
              } <- parseRequest req
  let essence =
        if action == "publish"
          then "news"
          else essence'
  handle@(Config.Handle {hConfig = config, hApi = api, hLogHandle = logHandle}) <-
    Config.new
  let essenceColumn = getEssenceColumn essence action config api
  let essenceFields = getEssenceFields essenceColumn api
  let listOfPairs = withoutEmpty $ parseFieldValue essenceFields queryMBS
  let paramsMsg = fromString . show $ getRequiredFields essenceColumn api
  accessArr <- getAccessArr queryMBS
  isConstraintsCorrect <-
    runUnderApp
      (execWApp $ isConstraintCorrect essenceColumn listOfPairs)
      handle
  let requestAnswer =
        if isPathRequestCorrect req api
          then if isMethodCorrect method action api
                 then if isAccess essence action accessArr api
                        then if isRequiredParams essenceColumn queryMBS api
                               then if getAll $
                                       isTypeParamsCorrect
                                         essenceColumn
                                         listOfPairs
                                      then if getAll isConstraintsCorrect
                                             then RequestAnswer
                                                    { reqAnswerBool = True
                                                    , reqAnswerResponse =
                                                        notFound
                                                    }
                                             else RequestAnswer
                                                    { reqAnswerBool = False
                                                    , reqAnswerResponse =
                                                        notFoundWith
                                                          "Bad values"
                                                    }
                                      else RequestAnswer
                                             { reqAnswerBool = False
                                             , reqAnswerResponse =
                                                 notFoundWith
                                                   "Incorrect type of params"
                                             }
                               else RequestAnswer
                                      { reqAnswerBool = False
                                      , reqAnswerResponse =
                                          notFoundWith paramsMsg
                                      }
                        else RequestAnswer
                               { reqAnswerBool = False
                               , reqAnswerResponse = notFound
                               }
                 else RequestAnswer
                        { reqAnswerBool = False
                        , reqAnswerResponse =
                            notFoundWith "Incorrect request method"
                        }
          else RequestAnswer
                 { reqAnswerBool = False
                 , reqAnswerResponse = notFoundWith "Incorrect request path"
                 }
  logDebug logHandle $ "Accesses of request " <> show accessArr
  pure
    RequestCorrect
      { reqAnswer = requestAnswer
      , reqCQueryMBS = queryMBS
      , reqCConfigHandle = handle
      }

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
