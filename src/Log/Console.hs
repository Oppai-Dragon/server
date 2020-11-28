module Log.Console
  ( logMsg
  , logDebug
  , logInfo
  , logWarning
  , logError
  , tryM
  , endM
  , infoIO
  , prettyLog
  , lastPrettyCallStack
  , myPrettyCallStack
  , myPrettyCallStackLines
  , myPrettySrcLoc
  ) where

import Data.Base
import Log.File
import Log.Handle
import Log.Level

import Control.Exception
import Control.Monad
import Data.List
import Debug.Trace
import GHC.Stack

-------------------------------------------------------------------------------
-- * Basic
logMsg -- Log a message using the given logger at a given level
 ::
     HasCallStack
  => Handle
  -> Level
  -> String -- The log text itself
  -> IO ()
logMsg (Handle path maybeLevel) level text = do
  time <- getTime
  let prettyLoc = lastPrettyCallStack callStack
  let msg = time <> "-" <> prettyLog level text <> "\n\t" <> prettyLoc <> "\n"
  case maybeLevel of
    Just currentLevel -> when (currentLevel <= level) $ writeLog path msg
    Nothing -> writeLog path msg

-------------------------------------------------------------------------------
-- * Utility Functions
logDebug, logInfo, logWarning, logError :: HasCallStack => Handle -> String -> IO ()
logDebug = (`logMsg` DEBUG)

logInfo = (`logMsg` INFO)

logWarning logHandle msg = do
  logMsg logHandle WARNING msg
  traceIO $ prettyLog WARNING msg
  traceIO $ lastPrettyCallStack callStack

logError logHandle msg = do
  logMsg logHandle ERROR msg
  traceIO $ prettyLog ERROR msg
  traceIO $ lastPrettyCallStack callStack

tryM :: IO a -> IO (Either SomeException a)
tryM = try

endM :: Handle -> IO ()
endM (Handle path _) = writeLog path "\n\n\n"

infoIO :: HasCallStack => String -> IO ()
infoIO msg = do
  traceIO $ prettyLog INFO msg
  traceIO $ lastPrettyCallStack callStack

-------------------------------------------------------------------------------
-- * Readables
prettyLog :: Level -> String -> String
prettyLog level text = "[" <> show level <> "] " <> text

lastPrettyCallStack :: CallStack -> String
lastPrettyCallStack = myPrettySrcLoc . snd . last . getCallStack

myPrettyCallStack :: CallStack -> String
myPrettyCallStack = intercalate "\n" . myPrettyCallStackLines

myPrettyCallStackLines :: CallStack -> [String]
myPrettyCallStackLines stack =
  case getCallStack stack of
    [] -> []
    arr -> map (\(_, srcLoc) -> '\t' : myPrettySrcLoc srcLoc) arr

myPrettySrcLoc :: SrcLoc -> String
myPrettySrcLoc srcLoc =
  let fileLoc = srcLocFile srcLoc
      startLine = show $ srcLocStartLine srcLoc
      startColumn = show $ srcLocStartCol srcLoc
      endLine = show $ srcLocEndLine srcLoc
      endColumn = show $ srcLocEndCol srcLoc
      moduleLoc = srcLocModule srcLoc
   in "Called at " <>
      fileLoc <>
      " " <>
      startLine <>
      ":" <>
      startColumn <> "-" <> endLine <> ":" <> endColumn <> " in " <> moduleLoc
