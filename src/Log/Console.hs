module Log.Console
  ( logM
  , debugM
  , infoM
  , warningM
  , errorM
  , tryM
  , prettyLog
  , prettyFileLog
  ) where

import Data.Base
import Log.File
import Log.Handle
import Log.Level

import Control.Exception
import Debug.Trace
import GHC.Stack

-------------------------------------------------------------------------------
-- * Basic
logM -- Log a message using the given logger at a given level
 ::
     HasCallStack
  => Handle
  -> Level
  -> String -- The log text itself
  -> IO ()
logM (Handle path maybeLevel) level text = do
  time <- getTime
  let prettyLoc = prettyFileLog callStack
  let msg = time <> "-" <> prettyLog level text <> "\n\t" <> prettyLoc <> "\n"
  case maybeLevel of
    Just currentLevel ->
      if currentLevel <= level
        then writeLog path msg
        else return ()
    Nothing -> writeLog path msg

-------------------------------------------------------------------------------
-- * Utility Functions
debugM, infoM, warningM, errorM :: HasCallStack => Handle -> String -> IO ()
debugM = (`logM` DEBUG)

infoM = (`logM` INFO)

warningM logHandle msg = do
  logM logHandle WARNING msg
  traceIO $ prettyLog WARNING msg
  traceIO $ prettyFileLog callStack

errorM logHandle msg = do
  logM logHandle ERROR msg
  traceIO $ prettyLog ERROR msg
  traceIO $ prettyFileLog callStack

tryM :: IO a -> IO (Either SomeException a)
tryM = try

-------------------------------------------------------------------------------
-- * Readables
prettyLog :: Level -> String -> String
prettyLog level text = "[" <> show level <> "] " <> text

prettyFileLog :: CallStack -> String
prettyFileLog stack =
  let srcLoc = snd . last $ getCallStack stack
      fileLoc = srcLocFile srcLoc
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
