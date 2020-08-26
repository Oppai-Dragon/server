module Data.SQL.ToValue
  ( sqlToValue
  , fromZip
  , sqlValuesToJsonValue
  , sqlValuesArrToObj
  , sqlValuesArrToValue
  ) where

import Config
import Data.Essence
import Data.MyValue as MyValue

import Data.Aeson
import Data.Aeson.Types (parseMaybe)

import qualified Data.HashMap.Strict as HM

import qualified Data.List as L
import qualified Data.Text as T

import Database.HDBC

sqlToValue :: SqlValue -> Value
sqlToValue SqlNull = Null
sqlToValue sqlValue =
  MyValue.toValue $ MyValue.fromStr (fromSql sqlValue :: String)

fromZip :: [T.Text] -> [Value] -> Object
fromZip = (HM.fromList .) . zip

sqlValuesToJsonValue :: Essence List -> [SqlValue] -> Config -> Value
sqlValuesToJsonValue (EssenceList name action _) sqlValues (Config conf) =
  let essenceFields =
        case parseMaybe (.: T.pack name) conf of
          Just (Object fieldsObj) -> HM.keys fieldsObj
          _ -> []
      fields =
        case [name, action] of
          ["person", "get"] -> L.delete "access_key" essenceFields
          _ -> essenceFields
      sqlValuesNeeded =
        case [name, action] of
          ["person", "get"] ->
            case L.elemIndex "access_key" essenceFields of
              Just index -> flip L.delete sqlValues $ (!!) sqlValues index
              Nothing -> sqlValues
          _ -> sqlValues
   in Object . fromZip fields $ map sqlToValue sqlValuesNeeded

sqlValuesArrToObj :: Int -> Essence List -> [[SqlValue]] -> Config -> Object
sqlValuesArrToObj _ _ [] _ = HM.empty
sqlValuesArrToObj n essenceList (sqlValues:sqlValuesArr) conf =
  let essenceField = T.pack $ elName essenceList <> show n
      essenceValue = sqlValuesToJsonValue essenceList sqlValues conf
   in HM.singleton essenceField essenceValue `HM.union`
      sqlValuesArrToObj (n + 1) essenceList sqlValuesArr conf

sqlValuesArrToValue :: Essence List -> [[SqlValue]] -> Config -> Value
sqlValuesArrToValue = ((Object .) .) . sqlValuesArrToObj 1
