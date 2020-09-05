module Data.Request
  ( queryFromObj
  , getQueryString
  ) where

import Data.MyValue

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import qualified Network.HTTP.Types.URI as HTTPUri
import qualified Network.Wai as Wai

queryFromObj :: A.Object -> HTTPUri.Query
queryFromObj =
  map
    (\(l, r) -> (BSC8.pack $ T.unpack l, Just . BSC8.pack . toStr $ fromValue r)) .
  HM.toList

getQueryString :: Wai.Request -> IO HTTPUri.Query
getQueryString req =
  case lookup "Content-Type" (Wai.requestHeaders req) of
    Just "application/json" ->
      Wai.strictRequestBody req >>= \bs ->
        case A.decode bs of
          Just hashMap -> return (queryFromObj hashMap)
          Nothing -> return []
    _ -> return (Wai.queryString req)
