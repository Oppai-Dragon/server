module Data.SQL.ToValue
    ( sqlToValue
    , fromZip
    , sqlValuesToJsonValue
    , sqlValuesArrToObj
    , sqlValuesArrToValue
    ) where

import           Config
import qualified Data.MyValue        as MyValue
import qualified Data.Value          as Value
import           Data.Essence
import           Data.Essence.Methods

import           Data.Aeson

import qualified Data.ByteString     as BS
import qualified Data.Char           as C

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified Data.Scientific     as S

import qualified Data.Text           as T
import qualified Data.List           as L

import           Database.HDBC
import           Database.HDBC.PostgreSQL

sqlToValue :: SqlValue -> Value
sqlToValue SqlNull = Null
sqlToValue sqlValue = MyValue.toValue $ MyValue.fromStr (fromSql sqlValue :: String)

fromZip :: [T.Text] -> [Value] -> Object
fromZip = (HM.fromList .) . zip

sqlValuesToJsonValue :: Essence List -> [SqlValue] -> Config -> Value
sqlValuesToJsonValue (EssenceList name action list) sqlValues conf =
    let
        essenceDB = getEssenceDB (T.pack name) (T.pack action) conf
        essenceFields = getEssenceFields essenceDB
        fields = case [name,action] of
            ["person","get"] -> L.delete "access_key" essenceFields
            _                -> essenceFields
        sqlValuesNeeded = case [name,action] of
            ["person","get"] ->
                case L.elemIndex "access_key" essenceFields of
                    Just index -> flip L.delete sqlValues $ (!!) sqlValues index
                    Nothing    -> sqlValues
            _                -> sqlValues
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