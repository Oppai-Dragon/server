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

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import qualified Network.Wai as Wai

import qualified Network.HTTP.Types as HTTPTypes

pathHandler :: Wai.Request -> IO Wai.Response
pathHandler req = do
  (isValidRequest, response, query, config) <- isRequestCorrect req
  let req' = req {Wai.queryString = query}
  if isValidRequest
    then runReaderT (evalStateT (essenceResponse req') mempty) config
    else pure response

getEssenceList :: Wai.Request -> ReaderT Config IO (Essence List)
getEssenceList req = do
  config <- ask
  api <- lift setApi
  let [essence', action] = Wai.pathInfo req
  let essence =
        if action == "publish"
          then "news"
          else essence'
  let queryMBS = Wai.queryString req
  let essenceDB = getEssenceDB essence action config api
  toEssenceList essenceDB queryMBS

addAccessKey :: Wai.Request -> StateT (Essence List) (ReaderT Config IO) ()
addAccessKey req = do
  api <- fromStateT setApi
  (EssenceList name action list) <- get
  let queryMBS = Wai.queryString req
  let access = getAccess (T.pack name) (T.pack action) api
  let isNeed = access > Everyone
  let accessKeyList =
        case lookup "access_key" queryMBS of
          Just (Just accessKey) -> [("access_key", fromBS accessKey)]
          _ -> []
  let essenceList = list <> accessKeyList
  when isNeed $ put (EssenceList name action essenceList)

setEssenceList :: Wai.Request -> StateT (Essence List) (ReaderT Config IO) ()
setEssenceList req = do
  essenceList <- lift $ getEssenceList req
  put essenceList

deleteAccessKey :: StateT (Essence List) (ReaderT Config IO) ()
deleteAccessKey = do
  modify $ deletePair "access_key"

essenceResponse ::
     Wai.Request -> StateT (Essence List) (ReaderT Config IO) Wai.Response
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
postEssenceResponse :: StateT (Essence List) (ReaderT Config IO) Wai.Response
postEssenceResponse = do
  let wrapResponse = pure . jsonResponse
  (EssenceList _ action _) <- get
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
getEssenceResponse :: StateT (Essence List) (ReaderT Config IO) Wai.Response
getEssenceResponse = jsonResponse <$> dbGet
