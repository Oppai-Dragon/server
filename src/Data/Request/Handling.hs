module Data.Request.Handling
    ( pathHandler
    ) where

import           Config
import           Data.Base                                      (ifElseThen)
import           Data.Handler
import           Data.MyValue                                   (bsToStr)
import           Data.Request.Control                           (isRequestCorrect)
import           Data.SQL.Actions
import           Data.SQL.ToValue                               (sqlValuesArrToValue,integerToValue)
import qualified Data.Empty                             as E
import           Data.Essence
import           Data.Essence.Methods
import           Data.Essence.Parse                             (fromQuery)
import           Data.Essence.Relations.Methods
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
    (isValidRequest, response) <- isRequestCorrect req config
    if isValidRequest
        then essenceResponse req
        else pure response

getEssenseList :: Request -> ReaderT Config IO (Essence List)
getEssenseList req =
    config <- ask
    let pathReq = pathInfo req
    let essence' = head pathReq
    let action = head $ tail pathReq
    api <- setApi
    let actionDB = getDBAction action api
    let essence = if action == "publish" then "news" else essence'
    let queryBS = queryString req
    let essenceDB = getEssenceDB essence actionDB config
    essenceList <- toEssenceList essenceDB queryBS
    return essenceList

essenceResponse :: Request -> ReaderT Config IO Response
essenceResponse req = do
    config <- ask
    essenceList <- getEssenseList req
    case requestMethod req of
        "POST"  -> execStateT postEssenceResponse essenceList
        "GET"   -> execStateT getEssenceResponse essenceList
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
    Essence name action hashMap <- get
    config <- getConfig
    let dbAction =
            ifElseThen
            [dbAction'=="create",fst $ ifExisteGetPairBoolQueryBS essence queryMBS config]
            [dbAction',dbAction',"edit"]
    let wrapResponse = pure . jsonResponse
    addRelationsFields <-
            fromIO
            $ fmap (<>)
            $ ifElseThen
            [isEssenceRelations essence config]
            [return [],getRelationsFields essence queryBS config]
    let newQueryBS =
            (<>)
            (snd $ ifExisteGetPairBoolQueryBS essence queryBS config)
            $ addRelationsFields queryBS
    updateEssence essence
    updateQueryBS newQueryBS
    case dbAction of
        "create" -> dbCreate >>= wrapResponse
        "edit"   -> dbEdit >>= wrapResponse
        "delete" -> dbDelete >>= wrapResponse
        _        -> pure notFound

---------------------------------Set via GET-------------------------------------------
getEssenceResponse :: StateT (Essence List) (ReaderT Config IO) Response
getEssenceResponse = do
    queryBS <- getQueryBS
    essence <- getEssence
    config <- getConfig
    addRelationsFields <-
            fromIO
            $ fmap (<>)
            $ ifElseThen
            [isEssenceRelations essence config]
            [return [],getRelationsFields essence queryBS config]
    updateQueryBS $ addRelationsFields queryBS
    value <- dbGet
    pure . jsonResponse $ value