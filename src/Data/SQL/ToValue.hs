module Data.SQL.ToValue
    ( myParseStrings
    , integerToValue
    , stringToValue
    , arrayToValue
    , sqlToValue
    , fromZip
    , sqlValuesToJsonValue
    , sqlValuesArrToObj
    , sqlValuesArrToValue
    ) where

import           Config
import           Data.Essence

import           Data.Aeson

import           Data.ByteString        hiding (map,init,tail,zip,length)
import           Data.Char                     (isDigit)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified Data.Scientific     as S

import qualified Data.Text           as T
import qualified Data.List           as L

import           Database.HDBC
import           Database.HDBC.PostgreSQL

myParseStrings :: String -> String
myParseStrings [] = []
myParseStrings (x:xs) =
    if x == ','
        then "\",\"" <> myParseStrings xs
        else x : myParseStrings xs

integerToValue :: Integer -> Value
integerToValue = Number . read . show

stringToValue :: String -> Value
stringToValue = String . T.pack

arrayToValue :: [Value] -> Value
arrayToValue = Array . V.fromList

sqlToValue :: SqlValue -> Value
sqlToValue (SqlString var) = stringToValue var
sqlToValue (SqlByteString varBS) =
    let
        string = read $ show varBS :: String
        varResult = init $ tail string
        varStrings = read $ "[\"" <> myParseStrings varResult <> "\"]" :: [String]
        varIntegers = read $ "[" <> varResult <> "]" :: [Integer]
    in case string of
        ('{':x:xs) ->
            if isDigit x
                then arrayToValue . map integerToValue $ varIntegers
                else arrayToValue . map stringToValue $ varStrings
        xs         -> sqlToValue (SqlString xs)
sqlToValue (SqlInteger var) = integerToValue var
sqlToValue (SqlBool var) = Bool var
sqlToValue (SqlLocalDate var) = String . T.pack . show $ var
sqlToValue SqlNull = Null

fromZip :: [T.Text] -> [Value] -> Object
fromZip = (HM.fromList .) . zip

sqlValuesToJsonValue :: Essence List -> [SqlValue] -> Config -> Value
sqlValuesToJsonValue (EssenceList name action list) sqlValues conf =
    let
        nameT = T.pack name
        fields = case [name,action] of
            ["person","create"] -> getEssenceFields nameT conf
            ["person",_]        -> L.delete "access_key" $ getEssenceFields nameT conf
            _                   -> getEssenceFields nameT conf
        sqlValuesNeeded =
            case L.elemIndex "access_key" (getEssenceFields nameT conf) of
                Just index -> flip L.delete sqlValues $ (!!) sqlValues index
                Nothing    -> sqlValues
    in Object . fromZip fields $ map sqlToValue sqlValuesNeeded

sqlValuesArrToObj :: Int -> Essence List -> [[SqlValue]] -> Config -> Object
sqlValuesArrToObj _ _       []                             _    = HM.empty
sqlValuesArrToObj n essenceList sql@(sqlValues : sqlValuesArr) conf =
    let
        essenceField = T.pack $ name essenceList <> show n
        essenceValue = sqlValuesToJsonValue essenceList sqlValues conf
    in
        HM.singleton essenceField essenceValue `HM.union`
        sqlValuesArrToObj (n+1) essenceList sqlValuesArr conf

sqlValuesArrToValue :: Essence List -> [[SqlValue]] -> Config -> Value
sqlValuesArrToValue = ((Object .) .) . sqlValuesArrToObj 1