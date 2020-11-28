module Data.Base.Aeson
  ( toTextArr
  , toStrArr
  , toIntArr
  , toText
  , toStr
  , toObj
  , fromObj
  , toInt
  , isNull
  , deleteFields
  , getValue
  , Data.Base.Aeson.toInteger
  , toStringKeys
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V

type Field = T.Text

toTextArr :: A.Value -> [T.Text]
toTextArr = fromMaybe [] . AT.parseMaybe A.parseJSON

toStrArr :: A.Value -> [String]
toStrArr = fromMaybe [] . AT.parseMaybe A.parseJSON

toIntArr :: A.Value -> [Int]
toIntArr = fromMaybe [] . AT.parseMaybe A.parseJSON

toText :: A.Value -> T.Text
toText = fromMaybe "" . AT.parseMaybe A.parseJSON

toStr :: A.Value -> String
toStr value =
  case value of
    A.Bool bool -> show bool
    A.String text -> T.unpack text
    A.Number _ -> show $ toInt value
    A.Array vector ->
      case V.toList vector of
        A.String _:_ -> show $ toStrArr value
        A.Number _:_ -> show $ toIntArr value
        _ -> ""
    _ -> ""

toObj :: A.Object -> [Field] -> A.Value
toObj =
  (A.Object .) . foldr (\field value -> HM.singleton field $ A.Object value)

fromObj :: A.Value -> A.Object
fromObj = fromMaybe HM.empty . AT.parseMaybe A.parseJSON

toInt :: A.Value -> Int
toInt = fromMaybe 1 . AT.parseMaybe A.parseJSON

isNull :: A.Value -> Bool
isNull result =
  case result of
    A.Null -> True
    _ -> False

deleteFields :: A.Object -> [Field] -> A.Object
deleteFields = foldr HM.delete

getValue :: [Field] -> A.Object -> A.Value
getValue [] obj = A.Object obj
getValue (field:rest) objOld =
  case AT.parseMaybe (A..: field) objOld of
    Just (A.Object objNew) -> getValue rest objNew
    Just value -> value
    Nothing -> A.Null

toInteger :: A.Value -> Integer
toInteger = fromMaybe 0 . AT.parseMaybe A.parseJSON

toStringKeys :: HM.HashMap T.Text a -> HM.HashMap String a
toStringKeys = HM.fromList . map (\(l, r) -> (T.unpack l, r)) . HM.toList
