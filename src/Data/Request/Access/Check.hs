module Data.Request.Access.Check
  ( isAccess
  ) where

import Config

import Data.Request.Access

import qualified Data.Text as T

type Action = T.Text

type Name = T.Text

isAccess :: Name -> Action -> [Access] -> Api -> Bool
isAccess essence action accessArr api =
  let access = getAccess essence action api
   in elem access accessArr
