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
    let action' = head . tail . pathInfo $ req
    let essence' = head $ pathInfo req
    let essence =
            ifElseThen
            [essence' == "draft",action == "publish"]
            [essence',essence',"news"]
    (isCorrect, response) <- isRequestCorrect req config
    (essenceResp,)  <- essenceResponse req
    pure $ ifElseThen [isCorrect] [response,essenceResp]

getEssence :: Request -> ReaderT Config IO Response
getEssence req =



updateActionAndEssence :: T.Text -> T.Text -> IO (T.Text,T.Text)
updateActionAndEssence essence action = do
    api <- setApi
    let actionDB = getDBAction action api
    let essenceNew = if action == "publish" then "news" else essence
    return (actionDB,essenceNew)

essenceResponse :: Request -> ReaderT Config IO (Response,ParsedRequest)
essenceResponse req = do
    config <- ask

    let queryBS = queryString req
    let essenceDB = getEssenceDB essence action config
    let parsedReq =
            (essenceDB,queryBS)
    obj <-
        lift $
        ifElseThen
        [isEssenceRelations essence config]
        [return HM.empty,relationsHandler essence queryBS config]
    case HM.toList obj of
        [("result", A.Number 0)] ->
            pure (notFound,(HM.empty,[]))
        _                      ->
            case requestMethod req of
                        "POST"  -> runStateT postEssenceResponse parsedReq
                        "GET"   -> runStateT getEssenceResponse parsedReq
                        _       -> pure (notFound,(HM.empty,[]))

-- | Build a successful JSON response
jsonResponse :: A.ToJSON a => a -> Response
jsonResponse = responseBuilder
    status200
    [(hContentType, "application/json")]
    . A.fromEncoding . A.toEncoding
---------------------------------Set via POST------------------------------------------
postEssenceResponse :: Handler Response
postEssenceResponse = do
    action <- getAction
    essence' <- getEssence
    let essence =
            ifElseThen
            [essence' == "draft",action == "publish"]
            [essence',essence',"news"]
    queryBS <- getQueryBS
    config <- getConfig
    let dbAction' = getApiDBMethod action config
    let dbAction =
            ifElseThen
            [dbAction'=="create",fst $ ifExisteGetPairBoolQueryBS essence queryBS config]
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
getEssenceResponse :: Handler Response
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