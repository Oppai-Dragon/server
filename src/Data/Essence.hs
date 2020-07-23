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

import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import Data.Text (Text,unpack,pack)

type Field    = String
type Value    = String
type List     = [(Field,Value)]
type Database = HashMap String List
type DB       = HashMap String Description

data family Essence a
data instance Essence [(Field,A.Value)] = EssenceValue String String [(Field,A.Value)]
data instance Essence Database = EssenceDatabase Text Text Database deriving Show
data instance Essence DB = EssenceDB
    { nameOf          :: Text
    , actionOf        :: Text
    , fieldsOf        :: DB
    } deriving Show
data instance Essence List = EssenceList
    { name          :: String
    , action        :: String
    , list          :: List
    }

instance Monoid (Essence List) where
    mempty = EssenceList "" "" []
instance Semigroup (Essence List) where
    EssenceList name1 action listOfPairs1 <> EssenceList name2 _ listOfPairs2 =
        let
            nameArr = name1 <> "," <> name2
            listArr = listOfPairs1 <> listOfPairs2
        in EssenceList nameArr action listArr

data Description = Description
    { valueTypeOf   :: MyValue
    , valueOf       :: Maybe VALUE
    , relationsOf   :: Maybe Relations
    , constraintOf  :: Maybe Constraint
    } deriving Show

data VALUE = NULL | NOT VALUE
    deriving Show
instance Read VALUE where
    readsPrec _ input = case input of
        "null"     -> [(NULL,"")]
        "not null" -> [(NOT NULL,"")]

data Relations = Relations
    { table :: Text
    , field :: Text
    } deriving Show
instance Read Relations where
    readsPrec _ input =
        let
            valueArr = words input
            table = pack $ valueArr !! 1
            field = pack $ valueArr !! 3
        in [(Relations table field,"")]

data Constraint = UNIQUE | PRIMARY | OnAction
    deriving Show
instance Read Constraint where
    readsPrec _ input = case input of
        "primary key"        -> [(PRIMARY,"")]
        "unique"             -> [(UNIQUE,"")]
        "delete with parent" -> [(OnAction,"")]