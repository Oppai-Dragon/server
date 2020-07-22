{-# LANGUAGE FlexibleInstances #-}
module Data.Essence
    ( Essence (..)
    , Database
    , DB
    , Description (..)
    , ValueExpect (..)
    , VALUE (..)
    , Relations (..)
    , Constraint (..)
    ) where

import Data.MyValue

import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text,unpack)

type Field    = String
type Value    = String
type List     = [(Field,Value)]
type Database = HashMap String List
type DB       = HashMap String Description

data Essence a = Essence
    { name          :: Text
    , action        :: Text
    , fields        :: a
    } deriving (Show, Eq)

instance Monoid (Essence List) where
    mempty = Essence "" "" []
instance Semigroup (Essence List) where
    Essence name1 action listOfPairs1 <> Essence name2 _ listOfPairs =
        let
            nameArr = name1 <> "," <> name2
            listArr = listOfPairs1 <> listOfPairs2
        Essence nameArr action listArr
instance Show (Essence List) where
    show (Essence name "create" listOfPairs) =
        let
            fields = parseOnlyFields listOfPairs
            values = parseOnlyValues listOfPairs
            nameStr = unpack name
        in create nameStr fields values
    show (Essence name action@("edit") listOfPairs)   =
        let
            oldEssence =
                "id=" <> (fromJust $ lookup "id" listOfPairs)
            newEssence = parseListOfPairs action listOfPairs
            nameStr = unpack name
        in edit nameStr oldEssence newEssence
    show (Essence name action@("get") listOfPairs)    =
        let
            fieldsAndValue = parseListOfPairs action listOfPairs
            fields = "*"
            nameStr = unpack name
        in get nameStr fields fieldsAndValue
    show (Essence name "delete" listOfPairs) =
        let
            essence =
                "id=" <> (fromJust $ lookup "id" listOfPairs)
            nameStr = unpack name
        in delete nameStr essence

data Description = Description
    { valueTypeOf   :: MyValue
    , valueOf       :: Maybe VALUE
    , relationsOf   :: Maybe Relations
    , constraintOf  :: Maybe Constraint
    } deriving Show

instance Show ValueExpect where
    show StrV    = "string"
    show StrArrV = "array string"
    show IntV    = "int"
    show IntArrV = "array int"
    show BoolV   = "bool"
instance Read ValueExpect where
    readsPrec _ input = case input of
        "int"           -> [(IntV,"")]
        "array int"     -> [(IntArrV,"")]
        "string"        -> [(StrV,"")]
        "array string"  -> [(StrArrV,"")]
        "bool"          -> [(BoolV,"")]
        "date"          -> [(StrV,"")]
        "uuid"          -> [(StrV,"")]

data VALUE = NULL | NOT VALUE
    deriving Show
instance Read VALUE where
    readsPrec _ input = case input of
        "null"     -> [(NULL,"")]
        "not null" -> [(NOT NULL,"")]

data Relations = Relations
    { table :: String
    , field :: String
    } deriving Show
instance Read Relations where
    readsPrec _ input =
        let
            valueArr = words input
            table = valueArr !! 1
            field = valueArr !! 3
        in [(Relations table field,"")]

data Constraint = UNIQUE | PRIMARY | OnAction
    deriving Show
instance Read Constraint where
    readsPrec _ input = case input of
        "primary key"        -> [(PRIMARY,"")]
        "unique"             -> [(UNIQUE,"")]
        "delete with parent" -> [(OnAction,"")]