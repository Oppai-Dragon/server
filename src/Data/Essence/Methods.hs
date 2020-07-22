module Data.Essence.Methods
    ( getEssenceDB
    , getHashMapDesctiprion
    , iterateHashMapDBList
    , setDescription
    , getEssenceDB'
    , getValueExpect
    , getMaybeDataField
    , parseListOfPairs
    , parseOnlyValues
    , parseOnlyFields
    , withoutEmpty
    , toList
    , parseBSValue
    , parseFieldValue
    , fromQuery
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
import           Data.List              (intercalate)

getEssenceDB :: T.Text -> T.Text -> Config -> Essence DB
getEssenceDB essence action conf =
    case getEssenceDB' essence action conf of
        Essence name action hashMapDB ->
            Essence name action $ getHashMapDesctiprion hashMapDB
        _                             ->
            Essence "" "" HM.empty

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
        valueExpect = getValueExpect $ fromJust $ lookup "type" description
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
            Essence essence action $ HM.fromList $ unpackObj obj
        _                 -> Essence "" "" HM.empty

getMaybeDataField :: Read a => Maybe String -> Maybe a
getMaybeDataField Nothing      = Nothing
getMaybeDataField (Just value) = Just $ read value

getValueExpect :: String -> ValueExpect
getValueExpect = read

parseListOfPairs :: String -> List -> String
parseListOfPairs "edit" = intercalate "," . map (\(l,r) -> l <> "=" <> r)
parseListOfPairs "get" = intercalate " AND " . map (\(l,r) -> l <> "=" <> r)
parseListOfPairs "delete" = intercalate " AND " . map (\(l,r) -> l <> "=" <> r)

parseOnlyValues :: List -> String
parseOnlyValues fieldValue =
    intercalate ","
    $ snd
    $ unzip fieldValue

parseOnlyFields :: List -> String
parseOnlyFields fieldValue =
    intercalate ","
    $ fst
    $ unzip fieldValue

--parseFildsValues ::
parseFildsValues func fieldValue = intercalate "," $ func $ unzip fieldValue

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

fromQuery ::
    T.Text -> T.Text -> [(BS.ByteString, Maybe BS.ByteString)] -> Config -> Essence List
fromQuery essence action queryBS conf =
    let
        essenceFields = map T.unpack $ getEssenceFields essence conf
        listOfPairs = toList $ parseFieldValue essenceFields queryBS
    in Essence essence action listOfPairs

toEssenceList :: Essence DB -> WriterT (Essence List) (ReaderT Config IO) ()
toEssenceList = do
    essenceDB@(Essence name action hashMap) <- get
    [case HM.lookup key hashMap of {Just description -> relationsOf description }| key <- HM.keys hashMap]