{-# LANGUAGE FlexibleInstances #-}
module Data.SQL.Actions where

import Data.Essence
import Data.Essence.Methods
import Data.Essence.Parse.Clause
import Data.MyValue
import Data.SQL

import Data.List
import Data.Maybe (fromJust)

import Control.Monad.Trans.Writer.CPS

instance ShowSQL (Essence List) where
    showSql (EssenceList name "create" listOfPairs) =
        let
            fields = parseOnlyFields listOfPairs
            values = parseOnlyValues listOfPairs
        in showSql (Insert name fields values)
    showSql (EssenceList name "edit" listOfPairs)   =
        let
            newsWhere = case lookup "draft_id" listOfPairs of
                Just draftId -> [Where ("draft_id",draftId)]
                Nothing -> case lookup "id" listOfPairs of
                    Just id -> [Where ("id",id)]
                    Nothing -> [Where ("id",MyInteger 0)]
            wherePart = case name of
                "news" -> newsWhere
                _      -> case lookup "id" listOfPairs of
                    Just id -> [Where ("id",id)]
                    Nothing -> [Where ("id",MyInteger 0)]
            setPart = map Set listOfPairs
        in showSql (Edit name setPart wherePart)
    showSql essenceList@(EssenceList name "get" list)   =
        let
            (EssenceClause nameList clauseList) =
                execWriter $ tell (EssenceClause [name] [])
                >> toEssenceClause essenceList
            uniqueNames = nub nameList
            getName = intercalate "," uniqueNames
            matchingClauses = if length uniqueNames == 1 then [] else matchEssence uniqueNames
        in showSql (Get getName (matchingClauses <> clauseList))
    showSql (EssenceList name "delete" listOfPairs) =
        let wherePart = [Where ("id",fromJust $ lookup "id" listOfPairs)]
        in showSql (Delete name wherePart)