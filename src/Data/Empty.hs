{-# LANGUAGE FlexibleInstances #-}

module Data.Empty
  ( Empty(..)
  ) where

import Data.MyValue

class Empty a where
  empty :: a
  isEmpty :: a -> Bool
  parseValue :: a -> String

instance Empty Integer where
  empty = 0
  isEmpty 0 = True
  isEmpty _ = False
  parseValue = show

instance Empty [Integer] where
  empty = [0]
  isEmpty [0] = True
  isEmpty _ = False
  parseValue thing = "ARRAY" <> show thing

instance Empty String where
  empty = "\'\'"
  isEmpty "\'\'" = True
  isEmpty _ = False
  parseValue thing =
    let parsed = "\'" <> thing <> "\'"
     in if isEmpty thing
          then empty
          else parsed

instance Empty [String] where
  empty = ["\'\'"]
  isEmpty ["\'\'"] = True
  isEmpty _ = False
  parseValue thing =
    let parsed =
          "ARRAY" <>
          map
            (\x ->
               if x == '\"'
                 then '\''
                 else x)
            (show thing)
     in if isEmpty thing
          then head empty
          else parsed

instance Empty Bool where
  empty = False
  isEmpty _ = False
  parseValue = show

instance Empty MyValue where
  empty = MyEmpty
  isEmpty (MyString value) = isEmpty value
  isEmpty (MyStrings value) = isEmpty value
  isEmpty (MyInteger value) = isEmpty value
  isEmpty (MyIntegers value) = isEmpty value
  isEmpty (MyBool value) = isEmpty value
  isEmpty (MyDate value) = isEmpty value
  isEmpty (MyNextval value) = isEmpty value
  isEmpty MyEmpty = True
  parseValue (MyString value) = parseValue value
  parseValue (MyStrings value) = parseValue value
  parseValue (MyInteger value) = parseValue value
  parseValue (MyIntegers value) = parseValue value
  parseValue (MyBool value) = parseValue value
  parseValue (MyDate value) = parseValue value
  parseValue (MyNextval value) = case value of
    'n':'e':'x':'t':'v':'a':'l':_ -> value
    _ -> parseValue value
  parseValue MyEmpty = "null"
