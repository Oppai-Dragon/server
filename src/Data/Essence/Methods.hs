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
import Data.Essence.Column
import Data.Essence.GetFields
import Data.MyValue

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]

type Field = String

addList :: List -> Essence List -> Essence List
addList list1 essenceList = essenceList <> mempty {elList = list1}

deletePair :: String -> Essence List -> Essence List
deletePair field essenceList@(EssenceList {elList = list}) =
  essenceList
    {elList = L.deleteBy (\(l1, _) (l2, _) -> l1 == l2) (field, MyEmpty) list}

getEssenceFields :: Essence Column -> Api -> [Field]
getEssenceFields EssenceColumn {eColName = "news", eColAction = "create"} _ =
  ["id"]
getEssenceFields essenceColumn api =
  let relationsTree = getRelationsTree (eColName essenceColumn) api
      relationFields = getRelationFields relationsTree
   in getFields essenceColumn L.\\ relationFields

getEssenceColumn :: T.Text -> T.Text -> Config -> Api -> Essence Column
getEssenceColumn essence apiAction (Config conf) (Api api) =
  let dbAction = getApiDBMethod apiAction $ Api api
      essenceColumn = mempty {eColName = essence, eColAction = dbAction}
   in case getValue [essence] conf of
        A.Object objFromConf ->
          case getValue [essence] api of
            A.Object objFromApi ->
              essenceColumn
                { eColHashMap =
                    iterateHMJson $ HM.unions [objFromConf, objFromApi]
                }
            _ -> essenceColumn {eColHashMap = iterateHMJson objFromConf}
        _ -> mempty

iterateHMJson :: A.Object -> HM.HashMap T.Text Column
iterateHMJson = HM.map setColumn . HM.delete "access_key"

setColumn :: A.Value -> Column
setColumn = fromMaybe defaultColumn . AT.parseMaybe A.parseJSON

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
toEssenceList essenceColumn@(EssenceColumn { eColName = name
                                           , eColAction = action
                                           }) queryMBS = do
  configHandle <- askUnderApp
  let essenceFields = getEssenceFields essenceColumn $ hApi configHandle
  let listOfPairs = withoutEmpty $ parseFieldValue essenceFields queryMBS
  let nameStr = T.unpack name
  let actioStr = T.unpack action
  pure EssenceList {elName = nameStr, elAction = actioStr, elList = listOfPairs}
