module Data.Value
    ( toTextArr
    , toStrArr
    , toText
    , toStr
    ) where

import           Data.Base

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified Data.Text           as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.List as L
import           Data.Scientific

toTextArr :: Value -> [T.Text]
toTextArr value = case value of
    Array vector -> map toText $ V.toList vector
    Object obj   -> HM.keys obj
    Null         -> []
    value        -> [toText value]

toStrArr :: Value -> [String]
toStrArr value = case value of
    Array vector -> map toStr $ V.toList vector
    Object obj   -> map T.unpack $ HM.keys obj
    Null         -> []
    value        -> [toStr value]

toText :: Value -> T.Text
toText value = case value of
    String text -> text
    Number num  -> T.pack . show $ scientificToInteger num
    Bool bool   -> T.pack $ show bool
    _           -> ""

toStr :: Value -> String
toStr value = case value of
    String text -> T.unpack text
    Number num  -> show $ scientificToInteger num
    Bool bool   -> show bool
    _           -> ""