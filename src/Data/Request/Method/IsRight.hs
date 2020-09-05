module Data.Request.Method.IsRight
  ( isMethodCorrect
  ) where

import Config
import Data.Base

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as T

type Action = T.Text

type Method = BS.ByteString

isMethodCorrect :: Method -> Action -> Api -> Bool
isMethodCorrect method action api =
  let methodT = T.pack $ BSC8.unpack method
      actions = getMethodActions methodT api
   in case findText action actions of
        Just _ -> True
        Nothing -> False
