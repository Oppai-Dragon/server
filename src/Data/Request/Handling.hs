module Data.Request.Handling
    ( pathHandler
    , getEssenceList
    , addAccessKey
    , setEssenceList
    , deleteAccessKey
    , essenceResponse
    , postEssenceResponse
    , getEssenceResponse
    ) where

import           Config
import           Data.Base              hiding (deletePair)
import           Data.Handler
import           Data.MyValue                                   (fromBS)
import           Data.Request
import           Data.Request.Control
import           Data.Request.Access
import           Data.SQL.Actions
import           Data.SQL.ToValue
import qualified Data.Empty                             as E
import           Data.Essence
import           Data.Essence.Methods
import           Data.Essence.RelationsTree.Methods
import           Database.Get
import           Database.Edit
import           Database.Create
import           Database.Delete

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
    (isValidRequest, response,query) <- isRequestCorrect req
    let req' = req {queryString = query}
    if isValidRequest
        then evalStateT (essenceResponse req') mempty
        else pure response

getEssenceList :: Request -> ReaderT Config IO (Essence List)
getEssenceList req = do
    config <- ask
    api <- lift setApi
    let [essence',action] = pathInfo req
    let essence = if action == "publish" then "news" else essence'
    let queryMBS = queryString req
    let essenceDB = getEssenceDB essence action config api
    essenceList <- toEssenceList essenceDB queryMBS
    return essenceList

addAccessKey :: Request -> StateT (Essence List) (ReaderT Config IO) ()
addAccessKey req = do
    api <- fromStateT setApi
    (EssenceList name action list) <- get
    let queryMBS = queryString req
    let access = getAccess (T.pack name) (T.pack action) api
    let isNeed = access > Everyone
    let accessKeyList = case lookup "access_key" queryMBS of
            Just (Just accessKey) -> [("access_key",fromBS accessKey)]
            _                     -> []
    let essenceList = list <> accessKeyList
    if isNeed
        then put (EssenceList name action essenceList)
        else return ()

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
    addAccessKey req
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