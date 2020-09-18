{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Essence
  ( Essence(..)
  , Database
  , List
  , Column(..)
  , ValueType(..)
  , NULL(..)
  , Relations(..)
  , Constraint(..)
  , Action(..)
  ) where

import Data.MyValue
import Data.Base
import Data.SQL

import qualified Data.Aeson as A
import Data.Char
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Field = String

type Value = String

type List = [(Field, MyValue)]

type Database = HM.HashMap Field [(Field, Value)]

data family Essence a

data instance  Essence (Clause String) = EssenceClause{ecName ::
                                                       [Field],
                                                       ecClauseList :: [Clause String]}
                                           deriving (Show, Eq)

data instance  Essence Database = EssenceDatabase T.Text Database
                                    deriving (Show, Eq)

data instance  Essence Column = EssenceColumn{edbName :: T.Text,
                                              edbAction :: T.Text,
                                              edbHashmap :: HM.HashMap Field Column}
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

data Column =
  Column
    { dValueType :: ValueType
    , dNull :: Maybe NULL
    , dDefault :: Maybe Default
    , dRelations :: Maybe Relations
    , dConstraint :: Maybe Constraint
    , dAction :: Maybe Action
    }
  deriving (Show, Eq)

instance A.FromJSON Column where
  parseJSON =
    A.withObject "From JSON Data.Essence.Column" $ \x ->
      Column <$> x A..: "type" <*> x A..:? "null" <*> x A..:? "default" <*> x A..:? "relations" <*>
      x A..:? "constraint" <*>
      x A..:? "action"

data ValueType
  = SMALLINT
  | BIGINT
  | SERIAL
  | BIGSERIAL
  | BOOLEAN
  | CHAR Int
  | VARCHAR Int
  | DATE
  | INTEGER
  | INT
  | TEXT
  | UUID
  deriving (Eq)

instance Show ValueType where
  show SMALLINT = "SMALLINT"
  show BIGINT = "BIGINT"
  show SERIAL = "SERIAL"
  show BIGSERIAL = "BIGSERIAL"
  show BOOLEAN = "BOOLEAN"
  show (CHAR x) = "CHAR(" <> show x <> ")"
  show (VARCHAR x) = "VARCHAR(" <> show x <> ")"
  show DATE = "DATE"
  show INTEGER = "INTEGER"
  show INT = "INT"
  show TEXT = "TEXT"
  show UUID = "UUID"

instance A.FromJSON ValueType where
  parseJSON =
    A.withText "From JSON Data.Essence.ValueType" $ \x ->
      case T.unpack x of
        "SMALLINT" -> pure SMALLINT
        "BIGINT" -> pure BIGINT
        "SERIAL" -> pure SERIAL
        "BIGSERIAL" -> pure BIGSERIAL
        "BOOLEAN" -> pure BOOLEAN
        'C':'H':'A':'R':'(':rest -> pure . CHAR . read $ takeWhile isDigit rest
        'V':'A':'R':'C':'H':'A':'R':'(':rest ->
          pure . VARCHAR . read $ takeWhile isDigit rest
        "DATE" -> pure DATE
        "INTEGER" -> pure INTEGER
        "INT" -> pure INT
        "TEXT" -> pure TEXT
        "UUID" -> pure UUID
        _ -> fail $ "Unknown ValueType: " <> T.unpack x

data NULL
  = NULL
  | NOT NULL
  deriving (Show, Eq)

instance A.FromJSON NULL where
  parseJSON =
    A.withText "From JSON Data.Essence.NULL" $ \x ->
      case x of
        "NULL" -> pure NULL
        "NOT NULL" -> pure $ NOT NULL
        _ -> fail $ "Unknown NULL: " <> T.unpack x

data Default =
  Default T.Text
  deriving (Show, Eq)

instance A.FromJSON Default where
  parseJSON =
    A.withText "From JSON Data.Essence.Default" $ \x ->
      pure . Default . T.tail $ T.dropWhile (/= ' ') x

data Relations =
  Relations
    { rTable :: T.Text
    , rField :: T.Text
    }
  deriving (Show, Eq)

instance A.FromJSON Relations where
  parseJSON =
    A.withText "From JSON Data.Essence.Relations" $ \x ->
      pure . fst . head . readsPrec 0 $ T.unpack x

instance Read Relations where
  readsPrec _ input =
    let valueArr = words input
        table = T.pack $ valueArr !! 1
        field = valueArr !! 2
        fieldT = T.pack . init . tail $ valueArr !! 2
     in if and [length valueArr == 3, head field == '(', last field == ')']
          then [(Relations table fieldT, "")]
          else error "Read Data.Essence.Relations"

data Constraint
  = UNIQUE
  | PRIMARY
  deriving (Show, Eq)

instance A.FromJSON Constraint where
  parseJSON =
    A.withText "From JSON Data.Essence.Constraint" $ \x ->
      case x of
        "UNIQUE" -> pure UNIQUE
        "PRIMARY KEY" -> pure PRIMARY
        _ -> fail $ "Unknown Constraint: " <> T.unpack x

data Action
  = OnDeleteCascade
  | OnDeleteRestrict
  deriving (Eq)

instance Show Action where
  show x =
    case x of
      OnDeleteCascade -> "ON DELETE CASCADE"
      OnDeleteRestrict -> "ON DELETE RESTRICT"

instance A.FromJSON Action where
  parseJSON =
    A.withText "From JSON Data.Essence.Action" $ \x ->
      case x of
        "ON DELETE CASCADE" -> pure OnDeleteCascade
        "ON DELETE RESTRICT" -> pure OnDeleteRestrict
        _ -> fail $ "Unknown Action: " <> T.unpack x
