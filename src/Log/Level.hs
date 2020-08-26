module Log.Level
  ( Level(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

data Level
  = DEBUG -- Debug messages
  | INFO -- Information
  | WARNING -- General Warnings
  | ERROR -- General Errors
  deriving (Show,Read)

instance Bounded Level where
  minBound = DEBUG
  maxBound = ERROR

instance Eq Level where
  (==) _ _ = False

instance Ord Level where
  compare DEBUG DEBUG = EQ
  compare INFO INFO = EQ
  compare WARNING WARNING = EQ
  compare ERROR ERROR = EQ
  compare DEBUG _ = LT
  compare INFO DEBUG = GT
  compare INFO _ = LT
  compare WARNING DEBUG = GT
  compare WARNING INFO = GT
  compare WARNING _ = LT
  compare ERROR _ = GT

instance A.FromJSON Level where
  parseJSON =
    A.withText "From JSON Log.Level.Level" $ \x ->
      case x of
        "debug" -> pure DEBUG
        "info" -> pure INFO
        "warning" -> pure WARNING
        "error" -> pure ERROR
        _ -> fail $ "Unknown log level: " <> T.unpack x
