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
    , GetFields (..)
    ) where

import Data.MyValue
import Data.Required
import Data.SQL

import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import Data.Text (Text,unpack,pack)


type Field    = String
type Action   = Text
type Value    = String
type List     = [(Field,MyValue)]
type Database = HashMap Field [(Field,Value)]
type DB       = HashMap Field Description

data family Essence a
data instance Essence (Clause String) = EssenceClause
    { nameList   :: [Field]
    , clauseList :: [Clause String]
    } deriving (Show,Eq)
data instance Essence Database =
    EssenceDatabase Text Text Database
    deriving (Show,Eq)
data instance Essence DB = EssenceDB
    { nameOf          :: Text
    , actionOf        :: Text
    , hashMapOf       :: DB
    } deriving (Show,Eq)
data instance Essence List = EssenceList
    { name          :: Field
    , action        :: Field
    , list          :: List
    } deriving (Show,Eq)
instance Monoid (Essence List) where
    mempty = EssenceList "" "" []
instance Semigroup (Essence List) where
    EssenceList name action list1 <> EssenceList _ _ list2 =
        EssenceList name action $ list1 <> list2

instance Monoid (Essence (Clause String)) where
    mempty = EssenceClause [] []
instance Semigroup (Essence (Clause String)) where
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

class GetFields a where
    getFields :: Essence DB -> a
    iterateHM :: [(Field,Description)] -> Action -> [a]
    iterateHMCreate,iterateHMGet,iterateHMEdit,iterateHMDelete ::
        [(Field,Description)] -> [a]