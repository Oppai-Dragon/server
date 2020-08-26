module Data.Request.Access
  ( Access(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

data Access
  = Admin
  | Author
  | Person
  | Everyone
  deriving (Eq, Show, Read)

instance Bounded Access where
  maxBound = Admin
  minBound = Everyone

instance Ord Access where
  compare Admin Admin = EQ
  compare Author Author = EQ
  compare Person Person = EQ
  compare Everyone Everyone = EQ
  compare Admin _ = GT
  compare Author Admin = LT
  compare Author _ = GT
  compare Person Admin = LT
  compare Person Author = LT
  compare Person _ = GT
  compare Everyone _ = LT

instance A.FromJSON Access where
  parseJSON =
    A.withText "From JSON Log.Level.Level" $ \x ->
      case x of
        "Admin" -> pure Admin
        "Author" -> pure Author
        "Person" -> pure Person
        "Everyone" -> pure Everyone
        _ -> fail $ "Unknown access: " <> T.unpack x
