module Log.Handle.Builder
  ( new
  ) where

import Config.Set
import Log.File
import Log.Handle

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

new :: IO Handle
new = do
  (Local local) <- setLocal
  logPath <- setLogPath
  let maybeLevel =
        AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) local
  return $ Handle logPath maybeLevel
