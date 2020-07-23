module Data.Essence.Methods
    ( addList
    , deletePair
    , getEssenceDB
    , getHashMapDesctiprion
    , iterateHashMapDBList
    , setDescription
    , getEssenceDB'
    , getMyValue
    , getMaybeDataField
    , parseListOfPairs
    , parseOnlyValues
    , parseOnlyFields
    , withoutEmpty
    , toList
    , parseBSValue
    , parseFieldValue
    , toEssenceList
    ) where

import Config

import Data.Essence
import Data.Essence.Parse
import Data.Empty
import Data.MyValue
import Data.FromValue

import Data.Maybe (fromJust)

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text       as T
import qualified Data.ByteString as BS
import qualified Data.List       as L

import           Control.Monad.Trans.Reader

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]

addList :: List -> Essence List -> Essence List
addList list1 (EssenceList name action list2) =
    EssenceList name action $ list1 <> list2

deletePair :: String -> Essence List -> Essence List
deletePair field essenceList@(EssenceList name action list) =
    EssenceList name action $ L.deleteBy (\(l1,_) (l2,_) -> l1==l2) (field,undefined) list

getEssenceDB :: T.Text -> T.Text -> Config -> Essence DB
getEssenceDB essence action conf =
    case getEssenceDB' essence action conf of
        EssenceDatabase name action hashMapDB ->
            EssenceDB name action $ getHashMapDesctiprion hashMapDB
        _                             ->
            EssenceDB "" "" HM.empty

getHashMapDesctiprion :: Database -> DB
getHashMapDesctiprion = HM.fromList . iterateHashMapDBList . HM.toList

iterateHashMapDBList :: [(String,List)] -> [(String,Description)]
iterateHashMapDBList []                         = []
iterateHashMapDBList (("access_key",_):rest)    = iterateHashMapDBList rest
iterateHashMapDBList ((field,description):rest) =
    (field,setDescription description) : iterateHashMapDBList rest

setDescription :: List -> Description
setDescription description =
    let
        valueExpect = getMyValue $ fromJust $ lookup "type" description
        value       = getMaybeDataField $ lookup "value" description
        relations   = getMaybeDataField $ lookup "relations" description
        constraint  = getMaybeDataField $ lookup "constraint" description
    in Description valueExpect value relations constraint

getEssenceDB' :: T.Text -> T.Text -> Config -> Essence Database
getEssenceDB' essence action conf =
    let unpackObj obj = do
            (field,value) <- HM.toList obj
            let resultArr = map parsePsql $ toStrArr value
            return (T.unpack field, resultArr)
    in case parseMaybe (.: essence) conf of
        Just (Object obj) ->
            EssenceDatabase essence action $ HM.fromList $ unpackObj obj
        _                 -> EssenceDatabase "" "" HM.empty

getMaybeDataField :: Read a => Maybe String -> Maybe a
getMaybeDataField Nothing      = Nothing
getMaybeDataField (Just value) = Just $ read value

getMyValue :: String -> MyValue
getMyValue = read

parseListOfPairs :: String -> List -> String
parseListOfPairs "edit" = L.intercalate "," . map (\(l,r) -> l <> "=" <> r)
parseListOfPairs "get" = L.intercalate " AND " . map (\(l,r) -> l <> "=" <> r)
parseListOfPairs "delete" = L.intercalate " AND " . map (\(l,r) -> l <> "=" <> r)
--parseListOfPairs "array" = L.intercalate " OR " . map (\(l,r) -> map (\x -> l <> "=" <>) r)

parseOnlyValues :: List -> String
parseOnlyValues fieldValue =
    L.intercalate ","
    $ snd
    $ unzip fieldValue

parseOnlyFields :: List -> String
parseOnlyFields fieldValue =
    L.intercalate ","
    $ fst
    $ unzip fieldValue

--parseFildsValues ::
parseFildsValues func fieldValue = L.intercalate "," $ func $ unzip fieldValue

withoutEmpty :: [(String, MyValue)] -> [(String, MyValue)]
withoutEmpty [] = []
withoutEmpty (x@(l,r):xs) =
    if isEmpty r
        then withoutEmpty xs
        else x : withoutEmpty xs

toList :: [(String, MyValue)] -> [(String, String)]
toList = map (\(l,r) -> (l,parseEmpty r)) . withoutEmpty

parseBSValue ::
    String -> [(String, Maybe BS.ByteString)]
    -> (String, MyValue)
parseBSValue field bss =
    case lookup field bss of
        Just (Just var) -> (field, chooseMyValue var)
        _               -> (field, MyEmpty)

parseFieldValue ::
    [String] -> [(BS.ByteString, Maybe BS.ByteString)]
    -> [(String, MyValue)]
parseFieldValue []             _   = []
parseFieldValue (field:fields) bss =
    let bss' = map (\(l,r) -> (bsToStr l,r)) bss
    in parseBSValue field bss' : parseFieldValue fields bss

toEssenceList :: Essence DB -> QueryMBS -> ReaderT Config IO (Essence List)
toEssenceList (EssenceDB name action hashMap) queryMBS = do
    config <- ask
    let essenceFields = map T.unpack $ getEssenceFields name config
    let listOfPairs = toList $ parseFieldValue essenceFields queryMBS
    let nameStr = T.unpack name
    let actioStr = T.unpack action
    pure $ EssenceList nameStr actioStr listOfPairs

