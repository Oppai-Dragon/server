module Data.Value
  ( toTextArr
  , toStrArr
  , toText
  , toStr
  , toObj
  , toInt
  , isNull
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Maybe
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

toTextArr :: A.Value -> [T.Text]
toTextArr = fromMaybe [] . AT.parseMaybe A.parseJSON

toStrArr :: A.Value -> [String]
toStrArr = fromMaybe [] . AT.parseMaybe A.parseJSON

toText :: A.Value -> T.Text
toText = fromMaybe "" . AT.parseMaybe A.parseJSON

toStr :: A.Value -> String
toStr = fromMaybe "" . AT.parseMaybe A.parseJSON

toObj :: A.Value -> A.Object
toObj = fromMaybe HM.empty . AT.parseMaybe A.parseJSON

toInt :: A.Value -> Int
toInt = fromMaybe 1 . AT.parseMaybe A.parseJSON

isNull :: A.Value -> Bool
isNull result =
  case result of
    A.Null -> True
    _ -> False
