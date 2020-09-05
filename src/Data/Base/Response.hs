module Data.Base.Response
  ( notFoundWith
  , notFound
  ) where

import qualified Data.ByteString.Builder.Internal as BSBuilder
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai

notFoundWith :: BSBuilder.Builder -> Wai.Response
notFoundWith = Wai.responseBuilder HTTPTypes.status404 []

notFound :: Wai.Response
notFound = notFoundWith "Not found"
