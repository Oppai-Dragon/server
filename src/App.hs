module App where

import Data.Request.Handling

import qualified Network.Wai as Wai

app :: Wai.Application
app req send = do
  response <- pathHandler req
  send response
