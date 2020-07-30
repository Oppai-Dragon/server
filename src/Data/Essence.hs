{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Essence
    ( Essence (..)
    , Database
    , DB
    , List
    , Description (..)
    , VALUE (..)
    , Relations (..)
    , Constraint (..)
    ) where

import Data.MyValue
import Data.SQL

import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import Data.Text (Text,unpack,pack)

type Field    = String
type Value    = String
type List     = [(Field,MyValue)]
type Database = HashMap Field List
type DB       = HashMap Field Description

data family Essence a
data instance Essence Clause = EssenceClause
    { nameList   :: [Field]
    , clauseList :: [Clause String]
    }
data instance Essence [(Field,A.Value)] =
    EssenceValue Field Field [(Field,A.Value)]
data instance Essence Database =
    EssenceDatabase Text Text Database
    deriving (Show,Eq)
data instance Essence DB = EssenceDB
    { nameOf          :: Text
    , actionOf        :: Text
    , fieldsOf        :: DB
    } deriving (Show,Eq)
data instance Essence List = EssenceList
    { name          :: Field
    , action        :: Field
    , list          :: List
    } deriving Eq

instance Monoid (Essence Clause) where
    mempty = EssenceClause [] []
instance Semigroup (Essence Clause) where
    EssenceClause name1 listOfPairs1 <> EssenceClause name2 listOfPairs2 =
        let
            name = name1 <> name2
            list = listOfPairs1 <> listOfPairs2
        in EssenceClause name list

data Description = Description
    { valueTypeOf   :: MyValue
    , valueOf       :: Maybe VALUE
    , relationsOf   :: Maybe Relations
    , constraintOf  :: Maybe Constraint
    } deriving (Show,Eq)

data VALUE = NULL | NOT VALUE
    deriving (Show,Eq)
instance Read VALUE where
    readsPrec _ input = case input of
        "null"     -> [(NULL,"")]
        "not null" -> [(NOT NULL,"")]

data Relations = Relations
    { table :: Text
    , field :: Text
    } deriving (Show,Eq)
instance Read Relations where
    readsPrec _ input =
        let
            valueArr = words input
            table = pack $ valueArr !! 1
            field = pack $ valueArr !! 3
        in [(Relations table field,"")]

data Constraint = UNIQUE | PRIMARY | OnAction
    deriving (Show,Eq)
instance Read Constraint where
    readsPrec _ input = case input of
        "primary key"        -> [(PRIMARY,"")]
        "unique"             -> [(UNIQUE,"")]
        "delete with parent" -> [(OnAction,"")]