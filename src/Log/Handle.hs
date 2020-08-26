module Log.Handle
  ( Handle(..)
  ) where

import Log.Level

data Handle =
  Handle
    { hLogPath :: FilePath
    , hLogLevel :: Maybe Level
    }
  deriving (Show, Eq)
