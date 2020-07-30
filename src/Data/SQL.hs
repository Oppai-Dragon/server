{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Data.SQL
    ( ShowSQL (..)
    , SqlQuery (..)
    , Clause (..)
    , clauseSequenceA
    , withoutManyWhere
    ) where

import Data.Base
import Data.Empty
import Data.MyValue

import Data.List

type Table = String
type Fields = String
type Values = String
type SqlRequest = String
type FieldValue = String

class ShowSQL a where
    groupSql :: Ord a => [a] -> [[a]]
    groupSql = groupBy ((ordToBool .) . compare)
    showSql :: a -> SqlRequest
    parseList :: a -> String
    unpack :: a -> String

data SqlQuery
    = Insert
        { insertTable  :: String
        , insertFields :: [String]
        , insertValues :: [MyValue]
        }
    | Edit
        { editTable   :: String
        , setClause   :: [Clause String]
        , whereClause :: [Clause String]
        }
    | Get
        { getTable   :: String
        , getClauses :: [Clause String]
        }
    | Delete
        { deleteTable :: String
        , wherePart   :: [Clause String]
        }
instance ShowSQL SqlQuery where
    showSql (Insert table fields values) =
        "INSERT INTO " <> table
        <> " (" <> intercalate "," fields
        <> ") VALUES (" <> (intercalate "," . map parseValue) values
        <> ");"
    showSql (Edit table setPart wherePart) =
        "UPDATE " <> table
        <> " " <> showSql setPart
        <> " " <> showSql wherePart
        <> ";"
    showSql (Get table clauses) =
        "SELECT * FROM " <> table
        <> " " <> showSql clauses
        <> ";"
    showSql (Delete table wherePart) =
        "DELETE FROM " <> table
        <> " " <> showSql wherePart
        <> ";"

data family Clause a
data instance Clause String
    = Set { pairSet :: (String, MyValue) }
    | Where { pairWhere :: (String, MyValue) }
    | Filter { filter :: String }
    | OrderBy { orderBy :: String }
    deriving (Show,Eq)
data instance Clause [String]
    = SetList { listSet :: [(String, String)] }
    | WhereList { listWhere :: [(String, String)] }
    | FilterList { listFilter :: [String] }
    | OrderByList { listOrderBy :: [String] }
    deriving (Show,Eq)
instance Ord (Clause String) where
    compare (Set _)     (Set _)     = EQ
    compare (Where _)   (Where _)   = EQ
    compare (Filter _)  (Filter _)  = EQ
    compare (OrderBy _) (OrderBy _) = EQ

    compare (Set _) _ = GT

    compare (Where _) (Set _) = LT
    compare (Where _) _       = GT

    compare (Filter _) (Set _) = LT
    compare (Filter _) _       = GT

    compare (OrderBy _) (Set _)    = LT
    compare (OrderBy _) (Where _)  = LT
    compare (OrderBy _) (Filter _) = LT
    compare (OrderBy _) _          = GT
instance ShowSQL (Clause String) where
    unpack (Set (field,myValue))     = show $ (field,parseValue myValue)
    unpack (Where (field,myValue))   = show $ (field,parseValue myValue)
    unpack (Filter x)                = show x
    unpack (OrderBy x)               = show x
instance ShowSQL [Clause String] where
    showSql = showSql . map clauseSequenceA . groupSql . sort

instance Ord (Clause [String]) where
    compare (SetList _)     (SetList _)     = EQ
    compare (WhereList _)   (WhereList _)   = EQ
    compare (FilterList _)  (FilterList _)  = EQ
    compare (OrderByList _) (OrderByList _) = EQ

    compare (SetList _) _ = LT

    compare (WhereList _) (SetList _) = GT
    compare (WhereList _) _           = LT

    compare (FilterList _) (SetList _)   = GT
    compare (FilterList _) (WhereList _) = GT
    compare (FilterList _) _             = LT

    compare (OrderByList _) (SetList _)    = GT
    compare (OrderByList _) (WhereList _)  = GT
    compare (OrderByList _) (FilterList _) = GT
    compare (OrderByList _) _              = LT

instance ShowSQL (Clause [String]) where
    showSql = parseList

    parseList (SetList [])   = []
    parseList (SetList list) = (<>) " SET " . intercalate "," $
        map (\(l,r) -> l <> "=" <> r) list

    parseList (WhereList [])   = []
    parseList (WhereList list) = (<>) " WHERE " . intercalate " AND " $
        map (\(l,r) -> l <> "=" <> r) list

    parseList (FilterList [])   = []
    parseList (FilterList list) = (<>) " WHERE " . intercalate " AND " $ list

    parseList (OrderByList [])   = []
    parseList (OrderByList list) =
        flip (<>) ")" . (<>) " ORDER BY (" . intercalate "," $ list

    unpack (SetList x)     = show x
    unpack (WhereList x)   = show x
    unpack (FilterList x)  = show x
    unpack (OrderByList x) = show x
instance ShowSQL [Clause [String]] where
    showSql = unwords . reverse . withoutManyWhere . reverse .
        words . unwords . map showSql . sort

-- Apply only to homogeneous list of Clause String
clauseSequenceA :: [Clause String] -> Clause [String]
clauseSequenceA clauseList = case clauseList of
    []         -> WhereList []
    arr@(x:xs)       -> case x of
        Set x     -> SetList $ map (read . unpack) arr
        Where x   -> WhereList $ map (read . unpack) arr
        Filter x  -> FilterList $ map (read . unpack) arr
        OrderBy x -> OrderByList $ map (read . unpack) arr

--Replaces redundant "WHERE" with "AND"
withoutManyWhere :: [String] -> [String]
withoutManyWhere arr =
    let
        helper 0 _    arr = arr
        helper n func arr = helper (n-1) func $ func arr
    in case elemIndices "WHERE" arr of
        []       -> arr
        [x]      -> arr
        x1:xs    -> helper (length xs) (replaceBy (=="WHERE") "AND") arr
        _        -> arr