module Log.Handle.Builder
  ( new
  ) where

import Config
import Log.File
import Log.Handle

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

new :: IO Handle
new = do
  (Config config) <- setConfig
  logPath <- setLogPath
  let maybeLevel =
        AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) config
  return $ Handle logPath maybeLevel
