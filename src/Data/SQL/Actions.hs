{-# LANGUAGE FlexibleInstances #-}
module Data.SQL.Actions where

import Data.Essence
import Data.Essence.Methods
import Data.SQL

import Data.Maybe (fromJust)

instance Show (Essence List) where
    show (EssenceList name "create" listOfPairs) =
        let
            fields = parseOnlyFields listOfPairs
            values = parseOnlyValues listOfPairs
        in create name fields values
    show (EssenceList name action@("edit") listOfPairs)   =
        let
            oldEssence =
                "id=" <> (fromJust $ lookup "id" listOfPairs)
            newEssence = parseListOfPairs action listOfPairs
        in edit name oldEssence newEssence
    show (EssenceList name action@("get") listOfPairs)    =
        let
            fieldsAndValue = parseListOfPairs action listOfPairs
            fields = "*"
        in get name fields fieldsAndValue
    show (EssenceList name "delete" listOfPairs) =
        let
            essence =
                "id=" <> (fromJust $ lookup "id" listOfPairs)
        in delete name essence