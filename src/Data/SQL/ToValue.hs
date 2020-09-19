module Data.SQL.ToValue
  ( sqlToValue
  , fromZip
  , sqlValuesToJsonValue
  , sqlValuesArrToObj
  , sqlValuesArrToValue
  ) where

import Config
import Data.Base
import Data.Essence
import Data.MyValue as MyValue

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Database.HDBC as HDBC

sqlToValue :: HDBC.SqlValue -> A.Value
sqlToValue HDBC.SqlNull = A.Null
sqlToValue sqlValue =
  MyValue.toValue $ MyValue.fromStr (HDBC.fromSql sqlValue :: String)

fromZip :: [T.Text] -> [A.Value] -> A.Object
fromZip = (HM.fromList .) . zip

sqlValuesToJsonValue :: Essence List -> [HDBC.SqlValue] -> Config -> A.Value
sqlValuesToJsonValue (EssenceList name action _) sqlValues (Config conf) =
  let essenceFields =
        case getValue [T.pack name] conf of
          A.Object fieldsObj -> HM.keys fieldsObj
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
   in A.Object . fromZip fields $ map sqlToValue sqlValuesNeeded

sqlValuesArrToObj ::
     Int -> Essence List -> [[HDBC.SqlValue]] -> Config -> A.Object
sqlValuesArrToObj _ _ [] _ = HM.empty
sqlValuesArrToObj n essenceList (sqlValues:sqlValuesArr) conf =
  let essenceField = T.pack $ elName essenceList <> show n
      essenceValue = sqlValuesToJsonValue essenceList sqlValues conf
   in HM.singleton essenceField essenceValue `HM.union`
      sqlValuesArrToObj (n + 1) essenceList sqlValuesArr conf

sqlValuesArrToValue :: Essence List -> [[HDBC.SqlValue]] -> Config -> A.Value
sqlValuesArrToValue = ((A.Object .) .) . sqlValuesArrToObj 1
