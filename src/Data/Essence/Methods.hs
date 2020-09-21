{-# LANGUAGE FlexibleInstances #-}

module Data.Essence.Methods
  ( addList
  , deletePair
  , getEssenceFields
  , getEssenceColumn
  , iterateHMJson
  , setColumn
  , parseOnlyValues
  , parseOnlyFields
  , withoutEmpty
  , parseBSValue
  , parseFieldValue
  , toEssenceList
  ) where

import Config

import Data.Base hiding (deletePair)
import Data.Empty
import Data.Essence
import Data.Essence.GetFields
import Data.MyValue

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]

type Field = String

addList :: List -> Essence List -> Essence List
addList list1 (EssenceList name action list2) =
  EssenceList name action $ list2 <> list1

deletePair :: String -> Essence List -> Essence List
deletePair field (EssenceList name action list) =
  EssenceList name action $
  L.deleteBy (\(l1, _) (l2, _) -> l1 == l2) (field, MyEmpty) list

getEssenceFields :: Essence Column -> Api -> [Field]
getEssenceFields (EssenceColumn "news" "create" _) _ = ["id"]
getEssenceFields essenceColumn api =
  let relationsTree = getRelationsTree (eColName essenceColumn) api
      relationFields = getRelationFields relationsTree
   in getFields essenceColumn L.\\ relationFields

getEssenceColumn :: T.Text -> T.Text -> Config -> Api -> Essence Column
getEssenceColumn essence apiAction (Config conf) (Api api) =
  let dbAction = getApiDBMethod apiAction $ Api api
   in case getValue [essence] conf of
        A.Object objFromConf ->
          case getValue [essence] api of
            A.Object objFromApi ->
              EssenceColumn essence dbAction . iterateHMJson $ HM.unions [objFromConf, objFromApi]
            _ -> EssenceColumn essence dbAction $ iterateHMJson objFromConf
        _ -> EssenceColumn "" "" HM.empty

iterateHMJson :: A.Object -> HM.HashMap T.Text Column
iterateHMJson = HM.map setColumn . HM.delete "access_key"

setColumn :: A.Value -> Column
setColumn v =
  case AT.parseMaybe A.parseJSON v of
    Just x -> x
    Nothing ->
      error $ "Data.Essence.Methods.setColumn - can't parse " <> show v

parseOnlyValues :: List -> [MyValue]
parseOnlyValues = map snd

parseOnlyFields :: List -> [Field]
parseOnlyFields = map fst

withoutEmpty :: [(Field, MyValue)] -> [(Field, MyValue)]
withoutEmpty [] = []
withoutEmpty (x@(_, r):xs) =
  if isEmpty r
    then withoutEmpty xs
    else x : withoutEmpty xs

parseBSValue :: Field -> [(Field, Maybe BS.ByteString)] -> (Field, MyValue)
parseBSValue field bss =
  case lookup field bss of
    Just (Just var) -> (field, fromBS var)
    _ -> (field, MyEmpty)

parseFieldValue ::
     [Field] -> [(BS.ByteString, Maybe BS.ByteString)] -> [(Field, MyValue)]
parseFieldValue [] _ = []
parseFieldValue (field:fields) bss =
  let bss' = map (\(l, r) -> (BSC8.unpack l, r)) bss
   in parseBSValue field bss' : parseFieldValue fields bss

toEssenceList :: Essence Column -> QueryMBS -> UnderApp (Essence List)
toEssenceList essenceColumn@(EssenceColumn name action _) queryMBS = do
  configHandle <- askUnderApp
  let essenceFields = getEssenceFields essenceColumn $ hApi configHandle
  let listOfPairs = withoutEmpty $ parseFieldValue essenceFields queryMBS
  let nameStr = T.unpack name
  let actioStr = T.unpack action
  pure $ EssenceList nameStr actioStr listOfPairs
