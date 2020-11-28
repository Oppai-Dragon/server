module Config.Exception
  ( trySetIO
  ) where

import Log

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

trySetIO :: HasCallStack => IO A.Object -> IO A.Object
trySetIO set = do
  logHandle <- Log.new
  result <- tryM set
  case result of
    Right obj -> logDebug logHandle "Success on setting object" >> return obj
    Left err -> logError logHandle (show err) >> return HM.empty
