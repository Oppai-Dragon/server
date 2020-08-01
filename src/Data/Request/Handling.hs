module Data.Request.Handling
    ( pathHandler
    ) where

import           Config
import           Data.Base                                      (ifElseThen)
import           Data.Handler
import           Data.MyValue                                   (fromBS)
import           Data.Request.Control
import           Data.SQL.Actions
import           Data.SQL.ToValue
import qualified Data.Empty                             as E
import           Data.Essence
import           Data.Essence.Methods
import           Data.Essence.RelationsTree.Methods
import           DataBase.Get
import           DataBase.Edit
import           DataBase.Create
import           DataBase.Delete

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class          (lift)
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import qualified Data.Aeson                             as A
import qualified Data.HashMap.Strict                    as HM

import           Data.ByteString (ByteString)

import qualified Data.List                              as L    (deleteBy)

import qualified Data.Text                              as T
import           Data.Text.Encoding

import           Database.HDBC                          as HDBC
import qualified Database.HDBC.PostgreSQL               as PSQL

import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import qualified Network.Wai.Handler.Warp               as Warp
import           Network.Wai.Parse

import           Network.HTTP.Types

import           System.IO.Unsafe                               (unsafePerformIO)

---------------------------Response helpers------------------------------------------
-- Common error responses
notFound :: Response
notFound = responseBuilder status404 [] "Not found"

pathHandler :: Request -> ReaderT Config IO Response
pathHandler req = do
    config <- ask
    (isValidRequest, response) <- isRequestCorrect req
    if isValidRequest
        then evalStateT (essenceResponse req) (EssenceList "" "" [])
        else pure response

getEssenceList :: Request -> ReaderT Config IO (Essence List)
getEssenceList req = do
    config <- ask
    api <- lift setApi
    let pathReq = pathInfo req
    let essence' = head pathReq
    let action = head $ tail pathReq
    let essence = if action == "publish" then "news" else essence'
    let access = getAccess essence action api
    let actionDB = getApiDBMethod action api
    let queryMBS = queryString req
    let essenceDB'' = getEssenceDB essence actionDB config api
    let essenceDB'  = ifEveryoneUpdate essenceDB'' access
    let essenceDB   = ifGetUpdate essenceDB'
    essenceList <- toEssenceList essenceDB queryMBS
    return essenceList

setEssenceList :: Request -> StateT (Essence List) (ReaderT Config IO) ()
setEssenceList req = do
    essenceList <- lift $ getEssenceList req
    put essenceList

deleteAccessKey :: StateT (Essence List) (ReaderT Config IO) ()
deleteAccessKey = do
    essenceList <- get
    put $ deletePair "access_key" essenceList

essenceResponse :: Request -> StateT (Essence List) (ReaderT Config IO) Response
essenceResponse req = do
    setEssenceList req
    relationsObj <- addRelationsFields
    deleteAccessKey
    if HM.null relationsObj
        then pure $ responseBuilder status404 [] "Bad Relations"
        else case requestMethod req of
            "POST"  -> postEssenceResponse
            "GET"   -> getEssenceResponse
            _       -> pure notFound

-- | Build a successful JSON response
jsonResponse :: A.ToJSON a => a -> Response
jsonResponse = responseBuilder
    status200
    [(hContentType, "application/json")]
    . A.fromEncoding . A.toEncoding
---------------------------------Set via POST------------------------------------------
postEssenceResponse :: StateT (Essence List) (ReaderT Config IO) Response
postEssenceResponse = do
    let wrapResponse = pure . jsonResponse
    (EssenceList name action list) <- get
    isExiste <- isNewsExiste
    case action of
        "create" -> if isExiste
            then dbEdit >>= wrapResponse
            else dbCreate >>= wrapResponse
        "edit"   -> dbEdit >>= wrapResponse
        "delete" -> dbDelete >>= wrapResponse
        _        -> pure notFound

---------------------------------Set via GET-------------------------------------------
getEssenceResponse :: StateT (Essence List) (ReaderT Config IO) Response
getEssenceResponse = dbGet >>= pure . jsonResponse