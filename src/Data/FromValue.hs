module Data.FromValue
    ( toTextArr
    , toStrArr
    , toText
    , toStr
    , toBS
    , toQueryBS
    , toInt
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified Data.Text           as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.List as L

toTextArr :: Value -> [T.Text]
toTextArr (Array vector) = map toText $ V.toList vector
toTextArr (Object obj)   = HM.keys obj
toTextArr _              = []

toStrArr :: Value -> [String]
toStrArr = map T.unpack . toTextArr

toText :: Value -> T.Text
toText (String text) = text

toStr :: Value -> String
toStr = T.unpack . toText

toBS :: Value -> ByteString
toBS value =
    case value of
        String text    ->
            encodeUtf8 text
        Number number  ->
            encodeUtf8
            $ T.pack
            $ takeWhile (/='.')
            $ show number
        Array vector   ->
            encodeUtf8
            $ T.pack
            $ filter (/='\"')
            $ show
            $ map toBS
            $ V.toList vector
        Bool bool      ->
            encodeUtf8
            $ T.pack
            $ show bool
        _              -> "\'\'"

toQueryBS :: Object -> [(ByteString, Maybe ByteString)]
toQueryBS obj =
    let
        iterateArr = HM.keys obj
        iterateKeys []         = []
        iterateKeys (key:keys) =
            case HM.lookup key obj of
                Just Null  -> iterateKeys keys
                Just value ->
                    (encodeUtf8 key, Just $ toBS value) :
                    iterateKeys keys
                _          -> []
    in iterateKeys iterateArr

toInt :: Value -> Int
toInt (Number num) = read . L.delete '.' . takeWhile (/='e') $ show num