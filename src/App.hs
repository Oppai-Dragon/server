module App where

import Config
import Data.Request.Handling

import           Network.Wai
import qualified Network.Wai.Handler.Warp               as Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Autohead

import           Control.Monad.Trans.Reader

app :: Application
app req send = do
    config <- setConfig
    response <- runReaderT (pathHandler req) config
    send response