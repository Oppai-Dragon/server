module Data.Essence.Methods
    ( addList
    , deletePair
    , getEssenceDB
    , getEssenceDB'
    , getHashMapDesctiprion
    , iterateHashMapDBList
    , setDescription
    , getMaybeDataField
    , getMyValue
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
import Data.Essence.Clause
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

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]
type Field = String

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

getHashMapDesctiprion :: Database -> DB
getHashMapDesctiprion = HM.fromList . iterateHashMapDBList . HM.toList

iterateHashMapDBList :: [(String,List)] -> [(String,Description)]
iterateHashMapDBList []                         = []
iterateHashMapDBList (("access_key",_):rest)    = iterateHashMapDBList rest
iterateHashMapDBList ((field,list):rest)        =
    (field,setDescription list) : iterateHashMapDBList rest

setDescription :: List -> Description
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

withoutEmpty :: [(Field, MyValue)] -> [(Field, MyValue)]
withoutEmpty [] = []
withoutEmpty (x@(l,r):xs) =
    if isEmpty r
        then withoutEmpty xs
        else x : withoutEmpty xs

toList :: [(Field, MyValue)] -> [(Field, String)]
toList = map (\(l,r) -> (l,parseEmpty r)) . withoutEmpty

parseClause :: (String,MyValue) ->
parseClause (clause,myValue) =
        let
            name = takeWhile (/='_') clause
            value = parseEmpty myValue
            valueStr = toStr myValue
        in case name of
            "filter" ->
                Filter $
                case field of
                    "created_id"    -> "date_of_creation=" <> value
                    "created_after" -> "date_of_creation>" <> value
                    "created_before"-> "date_of_creation<" <> value
                    "tag"           -> value <> "=ANY(tag_ids)"
                    "tags_in"       -> parseTagsIn value
                    "tags_all"      -> "tag_ids=ARRAY" <> value
                    "name"          -> "name=" <> parseSubStr valueStr
                    "content"       -> "content=" <> parseSubStr valueStr
                    "author_name"   -> parseAuthorName name valueStr

            "sort"   ->
                OrderBy $
                case value of
                    "author_name"      -> parseAuthorName name valueStr
                    "number_of_photos" -> "ARRAY_LENGTH(draft_optional_photos, 1) DESC, draft_main_photo"
                    "category_name"    -> "category.name"
                    _                  -> field
            "search" ->
                Where $
                case field of
                    "author_name" -> parseAuthorName name value
                    _             -> field (" ILIKE(" <> parseSearchStr value <> ")")
            _        ->

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
    let listOfPairs = toList $ parseFieldValue essenceFields queryMBS
    let nameStr = T.unpack name
    let actioStr = T.unpack action
    pure $ EssenceList nameStr actioStr listOfPairs

