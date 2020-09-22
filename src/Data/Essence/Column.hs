module Data.Essence.Column
  ( defaultColumn
  , Column(..)
  , ValueType(..)
  , NULL(..)
  , Default(..)
  , Relations(..)
  , Constraint(..)
  , Action(..)
  ) where

import qualified Data.Aeson as A
import Data.Char
import qualified Data.Text as T

defaultColumn :: Column
defaultColumn =
  Column
    { cValueType = INT
    , cNULL = Nothing
    , cDefault = Nothing
    , cRelations = Nothing
    , cConstraint = Nothing
    , cAction = Nothing
    }

data Column =
  Column
    { cValueType :: ValueType
    , cNULL :: Maybe NULL
    , cDefault :: Maybe Default
    , cRelations :: Maybe Relations
    , cConstraint :: Maybe Constraint
    , cAction :: Maybe Action
    }
  deriving (Show, Eq)

instance A.FromJSON Column where
  parseJSON =
    A.withObject "From JSON Data.Essence.Column" $ \x ->
      Column <$> x A..: "type" <*> x A..:! "null" <*> x A..:! "default" <*>
      x A..:! "relations" <*>
      x A..:! "constraint" <*>
      x A..:! "action"

data ValueType
  = SMALLINT
  | SMALLINT_ARR
  | BIGINT
  | BIGINT_ARR
  | SERIAL
  | BIGSERIAL
  | BOOLEAN
  | BOOLEAN_ARR
  | CHAR Int
  | CHAR_ARR Int
  | VARCHAR Int
  | VARCHAR_ARR Int
  | DATE
  | DATE_ARR
  | INTEGER
  | INTEGER_ARR
  | INT
  | INT_ARR
  | TEXT
  | TEXT_ARR
  | UUID
  deriving (Eq)

instance Show ValueType where
  show SMALLINT = "SMALLINT"
  show SMALLINT_ARR = "SMALLINT[]"
  show BIGINT = "BIGINT"
  show BIGINT_ARR = "BIGINT"
  show SERIAL = "SERIAL"
  show BIGSERIAL = "BIGSERIAL"
  show BOOLEAN = "BOOLEAN"
  show BOOLEAN_ARR = "BOOLEAN[]"
  show (CHAR x) = "CHAR(" <> show x <> ")"
  show (CHAR_ARR x) = "CHAR(" <> show x <> ")[]"
  show (VARCHAR x) = "VARCHAR(" <> show x <> ")"
  show (VARCHAR_ARR x) = "VARCHAR(" <> show x <> ")[]"
  show DATE = "DATE"
  show DATE_ARR = "DATE[]"
  show INTEGER = "INTEGER"
  show INTEGER_ARR = "INTEGER[]"
  show INT = "INT"
  show INT_ARR = "INT[]"
  show TEXT = "TEXT"
  show TEXT_ARR = "TEXT[]"
  show UUID = "UUID"

instance A.FromJSON ValueType where
  parseJSON =
    A.withText "From JSON Data.Essence.ValueType" $ \x ->
      case T.unpack x of
        "SMALLINT" -> pure SMALLINT
        "SMALLINT[]" -> pure SMALLINT_ARR
        "BIGINT" -> pure BIGINT
        "BIGINT[]" -> pure BIGINT_ARR
        "SERIAL" -> pure SERIAL
        "BIGSERIAL" -> pure BIGSERIAL
        "BOOLEAN" -> pure BOOLEAN
        "BOOLEAN[]" -> pure BOOLEAN_ARR
        'C':'H':'A':'R':'(':rest1 ->
          case reverse rest1 of
            ']':'[':')':rest2 ->
              if all isDigit rest2
                then pure . CHAR_ARR . read $ takeWhile isDigit rest1
                else error
                       "FROM JSON Data.Essence.ValueType cann't parse CHAR(x)[]"
            ')':rest2 ->
              if all isDigit rest2
                then pure . CHAR . read $ takeWhile isDigit rest1
                else error
                       "FROM JSON Data.Essence.ValueType cann't parse CHAR(x)"
            _ -> error "FROM JSON Data.Essence.ValueType cann't parse CHAR"
        'V':'A':'R':'C':'H':'A':'R':'(':rest1 ->
          case reverse rest1 of
            ']':'[':')':rest2 ->
              if all isDigit rest2
                then pure . VARCHAR_ARR . read $ takeWhile isDigit rest1
                else error
                       "FROM JSON Data.Essence.ValueType cann't parse VARCHAR(x)[]"
            ')':rest2 ->
              if all isDigit rest2
                then pure . VARCHAR . read $ takeWhile isDigit rest1
                else error
                       "FROM JSON Data.Essence.ValueType cann't parse VARCHAR(x)"
            _ -> error "FROM JSON Data.Essence.ValueType cann't parse VARCHAR"
        "DATE" -> pure DATE
        "DATE[]" -> pure DATE_ARR
        "INTEGER" -> pure INTEGER
        "INTEGER[]" -> pure INTEGER_ARR
        "INT" -> pure INT
        "INT[]" -> pure INT_ARR
        "TEXT" -> pure TEXT
        "TEXT[]" -> pure TEXT_ARR
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

newtype Default =
  Default T.Text
  deriving (Eq)

instance Show Default where
  show (Default text) = "DEFAULT " <> T.unpack text

instance A.FromJSON Default where
  parseJSON =
    A.withText "From JSON Data.Essence.Default" $ \x -> pure $ Default x

data Relations =
  Relations
    { rTable :: T.Text
    , rField :: T.Text
    }
  deriving (Eq)

instance Show Relations where
  show (Relations table field) =
    "REFERENCES " <> T.unpack table <> " (" <> T.unpack field <> ")"

instance A.FromJSON Relations where
  parseJSON =
    A.withText "From JSON Data.Essence.Relations" $ \x ->
      pure . fst . head . reads $ T.unpack x

instance Read Relations where
  readsPrec _ input =
    let valueArr = words input
        table = T.pack $ valueArr !! 1
        field = valueArr !! 2
        fieldT = T.pack . init . tail $ valueArr !! 2
     in if (length valueArr == 3) && (head field == '(') && (last field == ')')
          then [(Relations table fieldT, "")]
          else error "Read Data.Essence.Relations"

data Constraint
  = UNIQUE
  | PrimaryKey
  deriving (Eq)

instance Show Constraint where
  show x =
    case x of
      UNIQUE -> "UNIQUE"
      PrimaryKey -> "PRIMARY KEY"

instance A.FromJSON Constraint where
  parseJSON =
    A.withText "From JSON Data.Essence.Constraint" $ \x ->
      case x of
        "UNIQUE" -> pure UNIQUE
        "PRIMARY KEY" -> pure PrimaryKey
        _ -> fail $ "Unknown Constraint: " <> T.unpack x

data Action
  = OnDeleteCascade
  | OnDeleteRestrict
  | OnDeleteSetNull
  deriving (Eq)

instance Show Action where
  show x =
    case x of
      OnDeleteCascade -> "ON DELETE CASCADE"
      OnDeleteRestrict -> "ON DELETE RESTRICT"
      OnDeleteSetNull -> "ON DELETE SET NULL"

instance A.FromJSON Action where
  parseJSON =
    A.withText "From JSON Data.Essence.Action" $ \x ->
      case x of
        "ON DELETE CASCADE" -> pure OnDeleteCascade
        "ON DELETE RESTRICT" -> pure OnDeleteRestrict
        "ON DELETE SET NULL" -> pure OnDeleteSetNull
        _ -> fail $ "Unknown Action: " <> T.unpack x
