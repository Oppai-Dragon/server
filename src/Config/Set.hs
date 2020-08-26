module Config.Set
  ( Config(..)
  , Api(..)
  , Psql(..)
  , setPsql
  , setConfig
  , setApi
  , setGlobalConfigPath
  , setConfigPath
  , setPsqlPath
  , setApiPath
  , chooseConfig
  ) where

import Config.Internal
import Data.Base
import Data.Value

import qualified Data.HashMap.Strict as HM

setPsql :: IO Psql
setPsql = Psql <$> set setPsqlPath

setConfig :: IO Config
setConfig = Config <$> set setConfigPath

setApi :: IO Api
setApi = Api <$> set setApiPath

setGlobalConfigPath, setConfigPath, setPsqlPath, setApiPath :: IO FilePath
setGlobalConfigPath = setPath "GlobalConfig.json"

setConfigPath = setPath "Config.json"

setPsqlPath = setPath "Psql.json"

setApiPath = setPath "Api.json"

chooseConfig :: EssenceName -> IO Config
chooseConfig essence = do
  globalConfig <- set setGlobalConfigPath
  let localConfig = toObj $ getValue ["config"] globalConfig
  let essenceObj = toObj $ getValue [essence] globalConfig
  let config = HM.union localConfig essenceObj
  return $ Config config
