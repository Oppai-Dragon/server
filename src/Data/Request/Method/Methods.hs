module Data.Request.Method.Methods
    ( isFindAction
    , isMethodCorrect
    ) where

import Config

import Data.ByteString (ByteString)
import Data.Text (Text)

type Action = Text
type Actions = [Action]
type Method = ByteString

isFindAction :: Action -> Actions -> Bool
isFindAction _      []                = False
isFindAction action (actionX:actions) =
    if action == actionX
        then True
        else isFindAction action actions

isMethodCorrect :: Method -> Action -> Config -> Bool
isMethodCorrect method action conf =
    let methodActions = getMethodActions conf
    in case lookup method methodActions of
        Just actions -> isFindAction action actions
        Nothing      -> False