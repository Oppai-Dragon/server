module Log.File
  ( setLogPath
  , writeLog
  ) where

import Data.Base

import qualified Data.Time.Clock as UTC
import qualified System.Directory as Dir

setLogPath :: IO FilePath
setLogPath = do
  repPath <- getRepDir
  let logsDirPath = repPath <> "\\logs"
  Dir.createDirectoryIfMissing False logsDirPath
  (UTC.UTCTime day _) <- UTC.getCurrentTime
  let logFile = show day <> ".txt"
  let logPath = logsDirPath <> "\\" <> logFile
  return logPath

writeLog :: FilePath -> String -> IO ()
writeLog = appendFile
