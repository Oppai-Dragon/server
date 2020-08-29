module Config.Internal
  ( Action
  , EssenceName
  , Field
  , Config(..)
  , Api(..)
  , Psql(..)
  , Local(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

type Action = T.Text

type EssenceName = T.Text

type Field = T.Text

newtype Config =
  Config A.Object
  deriving (Show, Eq)

newtype Api =
  Api A.Object
  deriving (Show, Eq)

newtype Psql =
  Psql A.Object
  deriving (Show, Eq)

newtype Local =
  Local A.Object
  deriving (Show, Eq)
