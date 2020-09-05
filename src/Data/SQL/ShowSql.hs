{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Data.SQL.ShowSql
  ( ShowSql(..)
  , clauseSequenceA
  , withoutManyWhere
  ) where

import Data.Base
import Data.Empty
import Data.Essence
import Data.Essence.Methods
import Data.Essence.Parse.Clause
import Data.MyValue
import Data.SQL

import Data.Functor.Identity
import Data.List
import Data.Maybe

type SqlRequest = String

class ShowSql a where
  groupSql :: Ord a => [a] -> [[a]]
  groupSql = groupBy ((ordToBool .) . compare)
  showSql :: a -> SqlRequest

instance ShowSql SqlQuery where
  showSql (Insert table fields values) =
    "INSERT INTO " <>
    table <>
    " (" <>
    intercalate "," fields <>
    ") VALUES (" <> (intercalate "," . map parseValue) values <> ");"
  showSql (Edit table setPart part) =
    "UPDATE " <> table <> " " <> showSql setPart <> " " <> showSql part <> ";"
  showSql (Get table clauses) =
    "SELECT * FROM " <> table <> " " <> showSql clauses <> ";"
  showSql (Delete table part) =
    "DELETE FROM " <> table <> " " <> showSql part <> ";"

instance ShowSql (Clause String) where
  showSql = show

instance ShowSql [Clause String] where
  showSql = showSql . map clauseSequenceA . groupSql . sort

instance ShowSql (Clause [String]) where
  showSql (SetList []) = []
  showSql (SetList list) =
    (<>) " SET " . intercalate "," $ map (\(l, r) -> l <> "=" <> r) list
  showSql (WhereList []) = []
  showSql (WhereList list) =
    (<>) " WHERE " . intercalate " AND " $ map (\(l, r) -> l <> "=" <> r) list
  showSql (FilterList []) = []
  showSql (FilterList list) = (<>) " WHERE " . intercalate " AND " $ list
  showSql (OrderByList []) = []
  showSql (OrderByList list) =
    flip (<>) ")" . (<>) " ORDER BY (" . intercalate "," $ list
  showSql (OffsetLimitList []) = []
  showSql (OffsetLimitList (x:_)) = x

instance ShowSql [Clause [String]] where
  showSql =
    unwords .
    reverse . withoutManyWhere . reverse . words . unwords . map showSql . sort

instance ShowSql (Essence List) where
  showSql (EssenceList name "create" listOfPairs) =
    let fields = parseOnlyFields listOfPairs
        values = parseOnlyValues listOfPairs
     in showSql $ Insert name fields values
  showSql (EssenceList name "edit" listOfPairs) =
    let newsWhere =
          case lookup "draft_id" listOfPairs of
            Just draftId -> [Where ("draft_id", draftId)]
            Nothing ->
              case lookup "id" listOfPairs of
                Just idNum -> [Where ("id", idNum)]
                Nothing -> [Where ("id", MyInteger 0)]
        whereС =
          case name of
            "news" -> newsWhere
            _ ->
              case lookup "id" listOfPairs of
                Just idNum -> [Where ("id", idNum)]
                Nothing -> [Where ("id", MyInteger 0)]
        setPart = map Set listOfPairs
     in showSql $ Edit name setPart whereС
  showSql essenceList@(EssenceList name "get" _) =
    let (EssenceClause names clauseArr) =
          runIdentity . execWApp $
          tellWApp (EssenceClause [name] []) >> toEssenceClause essenceList
        uniqueNames = nub names
        getName = intercalate "," uniqueNames
        matchingClauses =
          case length uniqueNames of
            1 -> []
            _ -> matchEssence uniqueNames
     in showSql . Get getName $ matchingClauses <> clauseArr
  showSql (EssenceList name "delete" listOfPairs) =
    let myId = fromMaybe MyEmpty $ lookup "id" listOfPairs
        whereC = [Where ("id", myId)]
     in showSql $ Delete name whereC
  showSql x = show x

-- Apply only to homogeneous list of Clause String
clauseSequenceA :: [Clause String] -> Clause [String]
clauseSequenceA clauseArr =
  case clauseArr of
    [] -> WhereList []
    arr@(x:_) ->
      case x of
        Set _ ->
          SetList $
          map
            (\case
               Set (field, myValue) -> (field, parseValue myValue)
               _ -> ("", empty))
            arr
        Where _ ->
          WhereList $
          map
            (\case
               Where (field, myValue) -> (field, parseValue myValue)
               _ -> ("", empty))
            arr
        Filter _ ->
          FilterList $
          map
            (\case
               Filter field -> field
               _ -> "")
            arr
        OrderBy _ ->
          OrderByList $
          map
            (\case
               OrderBy field -> field
               _ -> "")
            arr
        OffsetLimit _ ->
          OffsetLimitList $
          map
            (\case
               OffsetLimit field -> field
               _ -> "")
            arr

--Replaces redundant "WHERE" with "AND"
withoutManyWhere :: [String] -> [String]
withoutManyWhere arr =
  let helper 0 _ xs = xs
      helper n func xs = helper (n - 1) func $ func xs
   in case elemIndices "WHERE" arr of
        _:xs -> helper (length xs) (replaceBy (== "WHERE") "AND") arr
        _ -> arr
