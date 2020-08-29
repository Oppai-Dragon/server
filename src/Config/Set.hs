module Config.Set
  ( Config(..)
  , Api(..)
  , Psql(..)
  , Local(..)
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

setPsql :: IO Psql
setPsql = Psql <$> set setPsqlPath

setApi :: IO Api
setApi = Api <$> set setApiPath

setConfig :: IO Config
setConfig = Config <$> set setConfigPath

setLocal :: IO Local
setLocal = Local <$> set setLocalPath

setConfigPath, setPsqlPath, setApiPath, setLocalPath :: IO FilePath
setConfigPath = setPath "Config.json"

setPsqlPath = setPath "Psql.json"

setApiPath = setPath "Api.json"

setLocalPath = setPath "Local.json"
