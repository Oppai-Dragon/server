{-# LANGUAGE FlexibleInstances #-}

module Data.Essence.Methods
  ( addList
  , deletePair
  , getEssenceFields
  , getEssenceColumn
  , getEssenceDatabase
  , getHashMapColumn
  , iterateHashMapDBList
  , setColumn
  , getMaybeDataField
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
import Data.Essence.Parse
import Data.MyValue

import Data.Maybe (fromJust)

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
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
  let relationsTree = getRelationsTree (edbName essenceColumn) api
      relationFields = getRelationFields relationsTree
   in getFields essenceColumn L.\\ relationFields

getEssenceColumn ::
     T.Text -> T.Text -> Config -> Api -> Essence Column
getEssenceColumn essence apiAction conf api =
  let dbAction = getApiDBMethod apiAction api
   in case getEssenceDatabase essence conf api of
        EssenceDatabase name hashMapDB ->
          EssenceColumn name dbAction $ getHashMapColumn hashMapDB

getEssenceDatabase :: T.Text -> Config -> Api -> Essence Database
getEssenceDatabase essence (Config conf) (Api api) =
  let unpackObj obj = do
        (field, value) <- HM.toList obj
        let resultArr = map parsePsql $ toStrArr value
        return (T.unpack field, resultArr)
   in case parseMaybe (.: essence) conf of
        Just (Object objFromConf) ->
          case parseMaybe (.: essence) api of
            Just (Object objFromApi) ->
              EssenceDatabase essence . HM.fromList $
              unpackObj objFromConf <> unpackObj objFromApi
            _ -> EssenceDatabase essence . HM.fromList $ unpackObj objFromConf
        _ -> EssenceDatabase "" HM.empty

getHashMapColumn :: Database -> HM.HashMap String Column
getHashMapColumn = HM.fromList . iterateHashMapDBList . HM.toList

iterateHashMapDBList ::
     [(String, [(String, String)])] -> [(String, Column)]
iterateHashMapDBList [] = []
iterateHashMapDBList (("access_key", _):rest) = iterateHashMapDBList rest
iterateHashMapDBList ((field, list):rest) =
  (field, setColumn list) : iterateHashMapDBList rest

setColumn :: [(String, String)] -> Column
setColumn list =
  let valueExpect = fst . last . reads . fromJust $ lookup "type" list
      value = getMaybeDataField $ lookup "value" list
      relations = getMaybeDataField $ lookup "relations" list
      constraint = getMaybeDataField $ lookup "constraint" list
   in Column valueExpect value relations constraint

getMaybeDataField :: Read a => Maybe String -> Maybe a
getMaybeDataField Nothing = Nothing
getMaybeDataField (Just value) =
  case reads value of
    [(l, _)] -> Just l
    _ -> Nothing

parseOnlyValues :: List -> [MyValue]
parseOnlyValues = map snd

parseOnlyFields :: List -> [String]
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
