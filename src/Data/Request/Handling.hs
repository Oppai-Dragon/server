module Data.Request.Handling
    ( pathHandler
    ) where

import           Config
import           Data.Base                                      (ifElseThen)
import           Data.Handler
import           Data.MyValue                                   (fromBS)
import           Data.Request.Control                           (isRequestCorrect)
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
        then essenceResponse req
        else pure response

getEssenceList :: Request -> ReaderT Config IO (Essence List)
getEssenceList req = do
    config <- ask
    let pathReq = pathInfo req
    let essence' = head pathReq
    let action = head $ tail pathReq
    api <- lift setApi
    let actionDB = getApiDBMethod action api
    let essence = if action == "publish" then "news" else essence'
    let queryBS = queryString req
    let essenceDB = getEssenceDB essence actionDB config api
    essenceList <- toEssenceList essenceDB queryBS
    return essenceList

essenceResponse :: Request -> ReaderT Config IO Response
essenceResponse req = do
    config <- ask
    essenceList@(EssenceList name action list) <- getEssenceList req
    relationObj <- evalStateT updateRelationsFields essenceList
    if HM.null relationObj
        then pure notFound
        else case requestMethod req of
            "POST"  -> evalStateT postEssenceResponse essenceList
            "GET"   -> evalStateT getEssenceResponse essenceList
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