module Data.Request.Access.Methods
    ( isAccess
    ) where

import Config

import Data.Request.Access

import Data.List
import Data.Text (Text)

type Action = Text
type Name = Text

isAccess :: Name -> Action -> [Access] -> Api -> Bool
isAccess essence action accessArr api =
    let access = getAccess essence action api
    in case find (access==) accessArr of
        Just access -> True
        Nothing     -> False