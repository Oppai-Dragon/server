{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Empty
    ( Empty (..)
    ) where

import Data.MyValue

class Empty a where
    empty :: a
    isEmpty :: a -> Bool
    parseEmpty :: a -> String
instance Empty Integer where
    empty = 0

    isEmpty 0 = True
    isEmpty _ = False

    parseEmpty = show
instance Empty [Integer] where
    empty = [0]

    isEmpty [0] = True
    isEmpty _   = False

    parseEmpty thing = "ARRAY" <> show thing
instance Empty String where
    empty = "\'\'"

    isEmpty "\'\'" = True
    isEmpty _      = False

    parseEmpty thing =
        let parsed = "\'" <> thing <> "\'"
        in if isEmpty thing
            then empty
            else parsed
instance Empty [String] where
    empty = ["\'\'"]

    isEmpty ["\'\'"] = True
    isEmpty _        = False

    parseEmpty thing =
        let parsed = "ARRAY" <> (map (\x -> if x == '\"' then '\'' else x) $ show thing)
        in if isEmpty thing
            then head empty
            else parsed
instance Empty Bool where
    empty = False

    isEmpty _     = False

    parseEmpty = show

instance Empty MyValue where
    isEmpty (MyString value)    = isEmpty value
    isEmpty (MyStrings value)   = isEmpty value
    isEmpty (MyInteger value)   = isEmpty value
    isEmpty (MyIntegers value)  = isEmpty value
    isEmpty (MyBool value)      = isEmpty value
    isEmpty (MyDate value)      = isEmpty value
    isEmpty (MyNextval value)   = isEmpty value
    isEmpty MyEmpty             = True

    parseEmpty (MyString value)     = parseEmpty value
    parseEmpty (MyStrings value)    = parseEmpty value
    parseEmpty (MyInteger value)    = parseEmpty value
    parseEmpty (MyIntegers value)   = parseEmpty value
    parseEmpty (MyBool value)       = parseEmpty value
    parseEmpty (MyDate value)       = parseEmpty value
    parseEmpty (MyNextval value)    = value