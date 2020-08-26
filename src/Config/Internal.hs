module Config.Internal
  ( Action
  , EssenceName
  , Field
  , Config(..)
  , Api(..)
  , Psql(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

type Action = T.Text

type EssenceName = T.Text

type Field = T.Text

newtype Config =
  Config A.Object

newtype Api =
  Api A.Object

newtype Psql =
  Psql A.Object
