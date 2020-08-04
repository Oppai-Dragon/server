{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Essence.Methods
    ( addList
    , deletePair
    , getEssenceFields
    , GetFields(..)
    , getEssenceDB
    , getEssenceDatabase
    , getHashMapDescription
    , iterateHashMapDBList
    , setDescription
    , getMaybeDataField
    , getMyValue
    , parseOnlyValues
    , parseOnlyFields
    , withoutEmpty
    , parseBSValue
    , parseFieldValue
    , toEssenceList
    ) where

import Config

import Data.Essence
import Data.Essence.Parse
import Data.Empty
import Data.MyValue
import Data.Value

import Data.Maybe (fromJust)

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.List             as L

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class (lift)

type QueryMBS    = [(BS.ByteString, Maybe BS.ByteString)]
type Field       = String
type Action      = T.Text
type EssenceName = String

addList :: List -> Essence List -> Essence List
addList list1 (EssenceList name action list2) =
    EssenceList name action $ list2 <> list1

deletePair :: String -> Essence List -> Essence List
deletePair field essenceList@(EssenceList name action list) =
    EssenceList name action
        $ L.deleteBy (\(l1,_) (l2,_) -> l1==l2) (field,MyEmpty) list

getEssenceFields :: Essence DB -> Api -> [Field]
getEssenceFields essenceDB api =
    let
        relationsTree = getRelationsTree (nameOf essenceDB) api
        relationFields = getRelationFields relationsTree
    in getFields essenceDB L.\\ relationFields

instance GetFields [Field] where
    getFields :: Essence DB -> [Field]
    getFields (EssenceDB name action hashMap) =
        let arr = iterateHM (HM.toList hashMap) action
        in concat arr

    iterateHM :: [(Field,Description)] -> Action -> [[Field]]
    iterateHM []  _      = []
    iterateHM arr action = case action of
        "create" -> iterateHMCreate arr
        "get"    -> ["page"] : iterateHMGet arr
        "edit"   -> iterateHMEdit arr
        "delete" -> iterateHMDelete arr
    iterateHMCreate,iterateHMGet,iterateHMEdit,iterateHMDelete ::
        [(Field,Description)] -> [[Field]]
    iterateHMCreate []                            = []
    iterateHMCreate ((field,description):rest)    =
        case field of
            "id"               -> iterateHMCreate rest
            "date_of_creation" -> iterateHMCreate rest
            _                  -> [field] : iterateHMCreate rest
    iterateHMGet = flip (:) [] . fst . unzip
    iterateHMEdit []                            = []
    iterateHMEdit ((field,description):rest)    =
        case field of
            "access_key"       -> iterateHMEdit rest
            "date_of_creation" -> iterateHMEdit rest
            _                  -> [field] : iterateHMEdit rest
    iterateHMDelete []                         = []
    iterateHMDelete ((field,description):rest) =
        case field of
            "id"         -> [[field]]
            _            -> iterateHMDelete rest

getEssenceDB :: T.Text -> T.Text -> Config -> Api -> Essence DB
getEssenceDB essence apiAction conf api =
    let dbAction = getApiDBMethod apiAction api
    in case getEssenceDatabase essence dbAction conf api of
        EssenceDatabase name dbAction hashMapDB ->
            EssenceDB name dbAction $ getHashMapDescription hashMapDB
        _                             ->
            EssenceDB "" "" HM.empty

getEssenceDatabase :: T.Text -> T.Text -> Config -> Api -> Essence Database
getEssenceDatabase essence action conf api =
    let unpackObj obj = do
            (field,value) <- HM.toList obj
            let resultArr = map parsePsql $ toStrArr value
            return (T.unpack field, resultArr)
    in case parseMaybe (.: essence) conf of
        Just (Object objFromConf) -> case parseMaybe (.: essence) api of
            Just (Object objFromApi) ->
                EssenceDatabase essence action . HM.fromList
                $ unpackObj objFromConf <> unpackObj objFromApi
            _                        ->
                EssenceDatabase essence action . HM.fromList
                $ unpackObj objFromConf
        _                 -> EssenceDatabase "" "" HM.empty

getHashMapDescription :: Database -> DB
getHashMapDescription = HM.fromList . iterateHashMapDBList . HM.toList

iterateHashMapDBList :: [(String,[(String,String)])] -> [(String,Description)]
iterateHashMapDBList []                         = []
iterateHashMapDBList (("access_key",_):rest)    = iterateHashMapDBList rest
iterateHashMapDBList ((field,list):rest)        =
    (field,setDescription list) : iterateHashMapDBList rest

setDescription :: [(String,String)] -> Description
setDescription list =
    let
        valueExpect = getMyValue $ fromJust $ lookup "type" list
        value       = getMaybeDataField $ lookup "value" list
        relations   = getMaybeDataField $ lookup "relations" list
        constraint  = getMaybeDataField $ lookup "constraint" list
    in Description valueExpect value relations constraint

getMaybeDataField :: Read a => Maybe String -> Maybe a
getMaybeDataField Nothing      = Nothing
getMaybeDataField (Just value) = Just $ read value

getMyValue :: String -> MyValue
getMyValue = read

parseOnlyValues :: List -> [MyValue]
parseOnlyValues = snd . unzip

parseOnlyFields :: List -> [String]
parseOnlyFields = fst . unzip

withoutEmpty :: [(Field, MyValue)] -> [(Field, MyValue)]
withoutEmpty [] = []
withoutEmpty (x@(l,r):xs) =
    if isEmpty r
        then withoutEmpty xs
        else x : withoutEmpty xs

parseBSValue ::
    Field -> [(Field, Maybe BS.ByteString)]
    -> (Field, MyValue)
parseBSValue field bss =
    case lookup field bss of
        Just (Just var) -> (field, fromBS var)
        _               -> (field, MyEmpty)

parseFieldValue ::
    [Field] -> [(BS.ByteString, Maybe BS.ByteString)]
    -> [(Field, MyValue)]
parseFieldValue []             _   = []
parseFieldValue (field:fields) bss =
    let bss' = map (\(l,r) -> (BSC8.unpack l,r)) bss
    in parseBSValue field bss' : parseFieldValue fields bss

toEssenceList :: Essence DB -> QueryMBS -> ReaderT Config IO (Essence List)
toEssenceList essenceDB@(EssenceDB name action hashMap) queryMBS = do
    config <- ask
    api <- lift setApi
    let essenceFields = getEssenceFields essenceDB api
    let listOfPairs = withoutEmpty $ parseFieldValue essenceFields queryMBS
    let nameStr = T.unpack name
    let actioStr = T.unpack action
    pure $ EssenceList nameStr actioStr listOfPairs