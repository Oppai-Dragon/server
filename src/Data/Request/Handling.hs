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

import Config
import Data.Base hiding (deletePair)
import Data.Essence
import Data.Essence.Methods
import Data.Essence.RelationsTree.Methods
import Data.MyValue (fromBS)
import Data.Request.Access
import Data.Request.Control
import Database.Create
import Database.Delete
import Database.Edit
import Database.Get
import Log

import Control.Monad

import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BSB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai

import qualified Network.HTTP.Types as HTTPTypes

pathHandler :: Wai.Request -> IO Wai.Response
pathHandler req = do
  (isValidRequest, response@(Wai.ResponseBuilder _ _ textBuilder), query, configHandle@(Config.Handle _ _ logHandle)) <-
    isRequestCorrect req
  let req' = req {Wai.queryString = query}
  let responseMsg = show $ BSB.toLazyByteString textBuilder
  if isValidRequest
    then runUnderApp (evalSApp (essenceResponse req') mempty) configHandle >>= \x ->
           endM logHandle >> pure x
    else debugM logHandle responseMsg >> endM logHandle >> pure response

getEssenceList :: Wai.Request -> UnderApp (Essence List)
getEssenceList req = do
  (Config.Handle config _ logHandle) <- askUnderApp
  api <- liftIO setApi
  let [essence', action] = Wai.pathInfo req
  let essence =
        if action == "publish"
          then "news"
          else essence'
  liftIO . debugM logHandle $
    "Path of request: " <> T.unpack action <> "/" <> T.unpack essence
  let queryMBS = Wai.queryString req
  liftIO . debugM logHandle $ "Query of request: " <> show queryMBS
  let essenceDB = getEssenceDB essence action config api
  toEssenceList essenceDB queryMBS

addAccessKey :: Wai.Request -> SApp ()
addAccessKey req = do
  api <- liftUnderApp $ liftIO setApi
  (EssenceList name action list) <- getSApp
  let queryMBS = Wai.queryString req
  let access = getAccess (T.pack name) (T.pack action) api
  let isNeed = access > Everyone
  let accessKeyList =
        case lookup "access_key" queryMBS of
          Just (Just accessKey) -> [("access_key", fromBS accessKey)]
          _ -> []
  let essenceList = list <> accessKeyList
  when isNeed $ putSApp (EssenceList name action essenceList)

setEssenceList :: Wai.Request -> SApp ()
setEssenceList req = do
  essenceList <- liftUnderApp $ getEssenceList req
  putSApp essenceList

deleteAccessKey :: SApp ()
deleteAccessKey = do
  modifySApp $ deletePair "access_key"

essenceResponse :: Wai.Request -> SApp Wai.Response
essenceResponse req = do
  setEssenceList req
  addAccessKey req
  relationsObj <- addRelationsFields
  deleteAccessKey
  if HM.null relationsObj
    then pure $ notFoundWith "Bad Relations"
    else case Wai.requestMethod req of
           "POST" -> postEssenceResponse
           "GET" -> getEssenceResponse
           _ -> pure notFound

-- | Build a successful JSON response
jsonResponse :: A.ToJSON a => a -> Wai.Response
jsonResponse =
  Wai.responseBuilder
    HTTPTypes.status200
    [(HTTPTypes.hContentType, "application/json")] .
  A.fromEncoding . A.toEncoding

---------------------------------Set via POST------------------------------------------
postEssenceResponse :: SApp Wai.Response
postEssenceResponse = do
  let wrapResponse = pure . jsonResponse
  (EssenceList _ action _) <- getSApp
  isExiste <- isNewsExiste
  case action of
    "create" ->
      if isExiste
        then dbEdit >>= wrapResponse
        else dbCreate >>= wrapResponse
    "edit" -> dbEdit >>= wrapResponse
    "delete" -> dbDelete >>= wrapResponse
    _ -> pure notFound

---------------------------------Set via GET-------------------------------------------
getEssenceResponse :: SApp Wai.Response
getEssenceResponse = jsonResponse <$> dbGet
