module Data.Base.Aeson
  ( getValue
  , scientificToInteger
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Text as T

getValue :: [T.Text] -> A.Object -> A.Value
getValue [] obj = A.Object obj
getValue (field:rest) objOld =
  case AT.parseMaybe (A..: field) objOld of
    Just (A.Object objNew) -> getValue rest objNew
    Just value -> value
    Nothing -> A.Null

scientificToInteger :: Scientific.Scientific -> Integer
scientificToInteger = fromMaybe 0 . AT.parseMaybe A.parseJSON . A.Number
