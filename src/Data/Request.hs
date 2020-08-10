module Data.Request
    ( queryFromObj
    , getQueryString
    ) where

import Data.Base
import Data.MyValue

import Data.Aeson
import qualified Data.HashMap.Strict   as HM
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text             as T

import Network.HTTP.Types.URI
import Network.Wai
import Network.Wai.Internal

queryFromObj :: Object -> Query
queryFromObj =
    map (\(l,r) -> (BSC8.pack $ T.unpack l,Just . BSC8.pack . toStr $ fromValue r))
    . HM.toList

getQueryString :: Request -> IO Query
getQueryString req = case lookup "Content-Type" (requestHeaders req) of
    Just "application/json" ->
        strictRequestBody req >>= \bs -> case decode bs of
            Just hashMap -> return (queryFromObj hashMap)
            Nothing      -> return []
    _                       -> return (queryString req)