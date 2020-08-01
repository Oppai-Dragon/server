{-# LANGUAGE FlexibleInstances #-}
module Data.Required
    ( Required (..)
    ) where

import Data.List

data Required a
    = Required [Required a]
    | AND a
    | OR  a
    | NullFields
instance Eq (Required [String]) where
    x1 == x2 = show x1 == show x2
instance Eq (Required String) where
    x1 == x2 = show x1 == show x2
instance Show (Required [String]) where
    show NullFields     = ""
    show (AND fields)   =
        "All of these fields need to be fill: "
        <> intercalate "," fields <> "."
    show (OR fields)    =
        "At least one of these fields need to be fill: "
        <> intercalate "," fields <> "."
    show (Required required) = concat $ map show required
instance Show (Required String) where
    show NullFields          = ""
    show (AND fields)        ="AND " <> fields
    show (OR fields)         ="OR " <> fields
    show (Required required) ="Required " <> show (map show required)
instance Functor Required where
    fmap func required = case required of
        AND fields   -> AND $ func fields
        OR fields    -> OR $ func fields
        Required arr -> Required $ map (fmap func) arr
        NullFields   -> NullFields