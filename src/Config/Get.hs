module Config.Get
  ( getApiActions
  , getEssences
  , getAccess
  , getUri
  , getUriDB
  , getMethodActions
  , getApiDBMethod
  , getRelationFields
  , getRequiredFields
  , getRelationsTree
  , getRelationsTree'
  , getOffsetLimit
  ) where

import Config.Internal
import Data.Base
import Data.Essence
import Data.Essence.GetFields
import Data.Essence.RelationsTree
import Data.Request.Access
import Data.Required

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T

getApiActions :: Api -> [Action]
getApiActions (Api api) =
  case getValue ["api"] api of
    A.Object obj -> HM.keys obj
    _ -> []

getEssences :: Api -> [T.Text]
getEssences (Api api) = toTextArr $ getValue ["essences"] api

getAccess :: EssenceName -> Action -> Api -> Access
getAccess essence action (Api api) =
  fromMaybe Admin . AT.parseMaybe A.parseJSON $
  getValue ["access", essence, action] api

getUri :: Config -> String
getUri (Config conf) = toStr $ getValue ["uriDB"] conf

getUriDB :: Psql -> String
getUriDB (Psql psql) =
  let getValue' field =
        case getValue [field] psql of
          A.String value -> value
          _ -> ""
      userName = getValue' "userName"
      password = getValue' "password"
      server = getValue' "server"
      port = getValue' "port"
      database = getValue' "database"
   in T.unpack $
      "postgresql://" <>
      userName <>
      ":" <> password <> "@" <> server <> ":" <> port <> "/" <> database

getMethodActions :: Field -> Api -> [Action]
getMethodActions method (Api api) = toTextArr $ getValue ["method", method] api

getApiDBMethod :: Action -> Api -> Action
getApiDBMethod action (Api api) = toText $ getValue ["api", action] api

getRelationFields :: RelationsTree -> [String]
getRelationFields relationsTree =
  case relationsTree of
    Root _ (Trunk _ tree) -> getRelationFields tree
    Root _ (Branch _ leafs) -> concatMap getRelationFields leafs
    Trunk _ tree -> getRelationFields tree
    Branch _ leafs -> concatMap getRelationFields leafs
    Leaf key -> [key]
    _ -> []

getRequiredFields :: Essence Description -> Api -> Required [String]
getRequiredFields (EssenceDescription "comment" "get" _) _ =
  Required [AND ["news_id"]]
getRequiredFields newsDB@(EssenceDescription "news" "create" _) _ =
  getFields newsDB
getRequiredFields essenceDescription api =
  let relationsTree = getRelationsTree (edbName essenceDescription) api
      relationFields = getRelationFields relationsTree
   in (L.\\ relationFields) <$> getFields essenceDescription

getRelationsTree :: EssenceName -> Api -> RelationsTree
getRelationsTree essence = getRelationsTree' essence 0 essence

getRelationsTree' :: EssenceName -> Int -> Field -> Api -> RelationsTree
getRelationsTree' essence n field (Api api) =
  let valueFind = getValue ["relations", essence] api
      valueArrFill name = getValue ["relations", essence, name] api
      getRootName = T.takeWhile (/= '_') . getRoot
      getRoot = head . HM.keys
      getLeafs = map (Leaf . T.unpack) . toTextArr . valueArrFill
   in case valueFind of
        A.Object obj ->
          getRelationsTree' (getRootName obj) 1 (getRoot obj) (Api api) <>
          if n == 0
            then Branch (T.unpack essence) (getLeafs $ getRoot obj)
            else Branch (T.unpack field) (getLeafs $ getRoot obj)
        A.String key -> Root (T.unpack field) . Leaf $ T.unpack key
        _ -> Ground

getOffsetLimit :: Int -> Psql -> String
getOffsetLimit pageCounter (Psql psql) =
  let pageLimit = toInt $ getValue ["page"] psql
      offset = pageLimit * (pageCounter - 1)
   in "OFFSET " <> show offset <> " LIMIT " <> show pageLimit
