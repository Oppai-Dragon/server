{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Essence
  ( Essence(..)
  , Database
  , DB
  , List
  , Description(..)
  , VALUE(..)
  , Relations(..)
  , Constraint(..)
  ) where

import Data.MyValue
import Data.SQL

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Field = String

type Value = String

type List = [(Field, MyValue)]

type Database = HM.HashMap Field [(Field, Value)]

type DB = HM.HashMap Field Description

data family Essence a

data instance  Essence (Clause String) = EssenceClause{ecName ::
                                                       [Field],
                                                       ecClauseList :: [Clause String]}
                                           deriving (Show, Eq)

data instance  Essence Database = EssenceDatabase T.Text Database
                                    deriving (Show, Eq)

data instance  Essence DB = EssenceDB{edbName :: T.Text,
                                      edbAction :: T.Text, edbHashmap :: DB}
                              deriving (Show, Eq)

data instance  Essence List = EssenceList{elName :: Field,
                                          elAction :: Field, elList :: List}
                                deriving (Show, Eq)

instance Monoid (Essence List) where
  mempty = EssenceList "" "" []

instance Semigroup (Essence List) where
  EssenceList name action list1 <> EssenceList _ _ list2 =
    EssenceList name action $ list1 <> list2

instance Monoid (Essence (Clause String)) where
  mempty = EssenceClause [] []

instance Semigroup (Essence (Clause String)) where
  EssenceClause name1 listOfPairs1 <> EssenceClause name2 listOfPairs2 =
    let name = name1 <> name2
        list = listOfPairs1 <> listOfPairs2
     in EssenceClause name list

data Description =
  Description
    { dValueType :: MyValue
    , dValue :: Maybe VALUE
    , dRelations :: Maybe Relations
    , dConstraint :: Maybe Constraint
    }
  deriving (Show, Eq)

data VALUE
  = NULL
  | NOT VALUE
  deriving (Show, Eq)

instance Read VALUE where
  readsPrec _ input =
    case input of
      "null" -> [(NULL, "")]
      "not null" -> [(NOT NULL, "")]
      _ -> [(NOT NULL, "")]

data Relations =
  Relations
    { rTable :: T.Text
    , rField :: T.Text
    }
  deriving (Show, Eq)

instance Read Relations where
  readsPrec _ input =
    let valueArr = words input
        table = T.pack $ valueArr !! 1
        field = T.pack $ valueArr !! 3
     in [(Relations table field, "")]

data Constraint
  = UNIQUE
  | PRIMARY
  deriving (Show, Eq)

instance Read Constraint where
  readsPrec _ input =
    case input of
      "primary key" -> [(PRIMARY, "")]
      "unique" -> [(UNIQUE, "")]
      _ -> []
