module Data.Request.Method.Methods
    ( isMethodCorrect
    ) where

import Config
import Data.Base

import qualified Data.ByteString.Char8 as BSC8
import Data.ByteString (ByteString)
import qualified Data.Text as T

type Action = T.Text
type Actions = [Action]
type Method = ByteString

isMethodCorrect :: Method -> Action -> Api -> Bool
isMethodCorrect method action api =
    let
        methodT = T.pack $ BSC8.unpack method
        actions = getMethodActions methodT api
    in case findText action actions of
        Just _  -> True
        Nothing -> False