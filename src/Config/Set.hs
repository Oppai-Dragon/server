module Config.Set
  ( Config(..)
  , Api(..)
  , Psql(..)
  , Local(..)
  , set
  , setPsql
  , setApi
  , setConfig
  , setLocal
  , setConfigPath
  , setPsqlPath
  , setApiPath
  , setLocalPath
  ) where

import Config.Internal
import Data.Base
import Log.Console
import Log.File
import Log.Handle

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BSL

set :: FilePath -> IO A.Object
set path = do
  logPath <- setLogPath
  result <- tryM $ BSL.readFile path
  case result of
    Right bsl ->
      case A.decode bsl of
        Just hm -> pure hm
        Nothing -> pure HM.empty
    Left err -> logError (Handle logPath Nothing) (show err) >> pure HM.empty

setPsql :: IO Psql
setPsql = (fmap Psql . set) =<< setPsqlPath

setApi :: IO Api
setApi = (fmap Api . set) =<< setApiPath

setConfig :: IO Config
setConfig = (fmap Config . set) =<< setConfigPath

setLocal :: IO Local
setLocal = (fmap Local . set) =<< setLocalPath

setConfigPath, setPsqlPath, setApiPath, setLocalPath :: IO FilePath
setConfigPath = setPath "configs/Config.json"

setPsqlPath = setPath "configs/Psql.json"

setApiPath = setPath "configs/Api.json"

setLocalPath = setPath "configs/Local.json"
