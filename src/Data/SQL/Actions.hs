{-# LANGUAGE FlexibleInstances #-}
module Data.SQL.Actions where

import Data.Essence
import Data.Essence.Methods
import Data.Essence.Parse.Clause
import Data.SQL

import Data.List
import Data.Maybe (fromJust)

instance ShowSql (Essence List) where
    showSql (EssenceList name "create" listOfPairs) =
        let
            fields = parseOnlyFields listOfPairs
            values = parseOnlyValues listOfPairs
        in showSql (Insert name fields values)
    showSql (EssenceList name action@("edit") listOfPairs)   =
        let
            wherePart = [Where ("id",fromJust $ lookup "id" listOfPairs)]
            setPart = map Set listOfPairs
        in showSql (Edit name setPart wherePart)
    showSql (EssenceList name action@("get") listOfPairs)    =
        let
            (EssenceClause nameList listClause) = parseClause listOfPairs
            getName = intercalate "," nameList
        in showSql (Get getName listClause)
    showSql (EssenceList name "delete" listOfPairs) =
        let wherePart = [Where ("id",fromJust $ lookup "id" listOfPairs)]
        in showSql (Delete name wherePart)