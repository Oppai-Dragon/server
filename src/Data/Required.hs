{-# LANGUAGE FlexibleInstances #-}
module Data.Required
    ( Required (..)
    ) where

import Data.Essence

import Data.List

data Required a
    = Required [Required a]
    | AND a
    | OR  a
    | NullFields
    deriving Eq
instance Show (Required [String]) where
    show NullFields     = ""
    show (AND fields)   =
        "All of these fields need to be fill: "
        <> intercalate "," fields <> "."
    show (OR fields)    =
        "At least one of these fields need to be fill: "
        <> intercalate "," fields <> "."
    show (Required required) = concat $ map show required
instance Functor Required where
    fmap func required = case required of
        AND fields   -> AND $ func fields
        OR fields    -> OR $ func fields
        Required arr -> Required $ map (fmap func) arr
        NullFields   -> NullFields