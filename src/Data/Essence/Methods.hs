module Data.Essence.Methods
    ( addList
    , deletePair
    , getEssenceDB
    , getEssenceDB'
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

type QueryMBS    = [(BS.ByteString, Maybe BS.ByteString)]
type Field       = String
type EssenceName = String

addList :: List -> Essence List -> Essence List
addList list1 (EssenceList name action list2) =
    EssenceList name action $ list1 <> list2

deletePair :: String -> Essence List -> Essence List
deletePair field essenceList@(EssenceList name action list) =
    EssenceList name action
        $ L.deleteBy (\(l1,_) (l2,_) -> l1==l2) (field,MyEmpty) list

getEssenceDB :: T.Text -> T.Text -> Config -> Api -> Essence DB
getEssenceDB essence action conf api =
    case getEssenceDB' essence action conf api of
        EssenceDatabase name action hashMapDB ->
            EssenceDB name action $ getHashMapDescription hashMapDB
        _                             ->
            EssenceDB "" "" HM.empty

getEssenceDB' :: T.Text -> T.Text -> Config -> Api -> Essence Database
getEssenceDB' essence action conf api =
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
toEssenceList (EssenceDB name action hashMap) queryMBS = do
    config <- ask
    let essenceFields = map T.unpack $ getEssenceFields name config
    let listOfPairs = withoutEmpty $ parseFieldValue essenceFields queryMBS
    let nameStr = T.unpack name
    let actioStr = T.unpack action
    pure $ EssenceList nameStr actioStr listOfPairs

