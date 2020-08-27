{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.SQL.ShowSql
  ( ShowSQL(..)
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

import Data.Maybe

import Control.Monad.Trans.Writer.CPS

import Data.List

type SqlRequest = String

class ShowSQL a where
  groupSql :: Ord a => [a] -> [[a]]
  groupSql = groupBy ((ordToBool .) . compare)
  showSql :: a -> SqlRequest
  parseList :: a -> String
  unpack :: a -> String

instance ShowSQL SqlQuery where
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
  parseList = showSql
  unpack = showSql

instance ShowSQL (Clause String) where
  showSql = parseList
  parseList (Set (field, myValue)) =
    parseList $ SetList [(field, parseValue myValue)]
  parseList (Where (field, myValue)) =
    parseList $ WhereList [(field, parseValue myValue)]
  parseList (Filter x) = parseList $ FilterList [x]
  parseList (OrderBy x) = parseList $ OrderByList [x]
  parseList (OffsetLimit x) = parseList $ OffsetLimitList [x]
  unpack (Set (field, myValue)) = show (field, parseValue myValue)
  unpack (Where (field, myValue)) = show (field, parseValue myValue)
  unpack (Filter x) = show x
  unpack (OrderBy x) = show x
  unpack (OffsetLimit x) = show x

instance ShowSQL [Clause String] where
  showSql = showSql . map clauseSequenceA . groupSql . sort
  parseList = show . map parseList
  unpack = show . map unpack

instance ShowSQL (Clause [String]) where
  showSql = parseList
  parseList (SetList []) = []
  parseList (SetList list) =
    (<>) " SET " . intercalate "," $ map (\(l, r) -> l <> "=" <> r) list
  parseList (WhereList []) = []
  parseList (WhereList list) =
    (<>) " WHERE " . intercalate " AND " $ map (\(l, r) -> l <> "=" <> r) list
  parseList (FilterList []) = []
  parseList (FilterList list) = (<>) " WHERE " . intercalate " AND " $ list
  parseList (OrderByList []) = []
  parseList (OrderByList list) =
    flip (<>) ")" . (<>) " ORDER BY (" . intercalate "," $ list
  parseList (OffsetLimitList []) = []
  parseList (OffsetLimitList (x:_)) = x
  unpack (SetList x) = show x
  unpack (WhereList x) = show x
  unpack (FilterList x) = show x
  unpack (OrderByList x) = show x
  unpack (OffsetLimitList x) = show x

instance ShowSQL [Clause [String]] where
  showSql =
    unwords .
    reverse . withoutManyWhere . reverse . words . unwords . map showSql . sort
  parseList = show . map parseList
  unpack = show . map unpack

instance ShowSQL (Essence List) where
  showSql (EssenceList name "create" listOfPairs) =
    let fields = parseOnlyFields listOfPairs
        values = parseOnlyValues listOfPairs
     in showSql (Insert name fields values)
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
     in showSql (Edit name setPart whereС)
  showSql essenceList@(EssenceList name "get" _) =
    let (EssenceClause names clauseArr) =
          execWriter $
          tell (EssenceClause [name] []) >> toEssenceClause essenceList
        uniqueNames = nub names
        getName = intercalate "," uniqueNames
        matchingClauses =
          if length uniqueNames == 1
            then []
            else matchEssence uniqueNames
     in showSql (Get getName (matchingClauses <> clauseArr))
  showSql (EssenceList name "delete" listOfPairs) =
    let whereC = [Where ("id", fromJust $ lookup "id" listOfPairs)]
     in showSql (Delete name whereC)
  showSql _ = "Bad argumant in showSql"
  parseList (EssenceList _ _ list) = show list
  unpack = parseList

-- Apply only to homogeneous list of Clause String
clauseSequenceA :: [Clause String] -> Clause [String]
clauseSequenceA clauseArr =
  case clauseArr of
    [] -> WhereList []
    arr@(x:_) ->
      case x of
        Set _ -> SetList $ map (read . unpack) arr
        Where _ -> WhereList $ map (read . unpack) arr
        Filter _ -> FilterList $ map (read . unpack) arr
        OrderBy _ -> OrderByList $ map (read . unpack) arr
        OffsetLimit _ -> OffsetLimitList $ map (read . unpack) arr

--Replaces redundant "WHERE" with "AND"
withoutManyWhere :: [String] -> [String]
withoutManyWhere arr =
  let helper 0 _ xs = xs
      helper n func xs = helper (n - 1) func $ func xs
   in case elemIndices "WHERE" arr of
        _:xs -> helper (length xs) (replaceBy (== "WHERE") "AND") arr
        _ -> arr
