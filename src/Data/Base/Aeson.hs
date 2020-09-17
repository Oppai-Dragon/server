module Data.Base.Aeson
  ( toTextArr
  , toStrArr
  , toIntArr
  , toText
  , toStr
  , toObj
  , toInt
  , isNull
  , getValue
  , scientificToInteger
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

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

toObj :: A.Value -> A.Object
toObj = fromMaybe HM.empty . AT.parseMaybe A.parseJSON

toInt :: A.Value -> Int
toInt = fromMaybe 1 . AT.parseMaybe A.parseJSON

isNull :: A.Value -> Bool
isNull result =
  case result of
    A.Null -> True
    _ -> False

getValue :: [T.Text] -> A.Object -> A.Value
getValue [] obj = A.Object obj
getValue (field:rest) objOld =
  case AT.parseMaybe (A..: field) objOld of
    Just (A.Object objNew) -> getValue rest objNew
    Just value -> value
    Nothing -> A.Null

scientificToInteger :: Scientific.Scientific -> Integer
scientificToInteger = fromMaybe 0 . AT.parseMaybe A.parseJSON . A.Number
