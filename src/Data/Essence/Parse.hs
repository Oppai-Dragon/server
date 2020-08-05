module Data.Essence.Parse
    ( parsePsql
    , parsePsqlExpr
    , parseType
    , parseValue
    , parseRelations
    , parseConstraint
    , parseDate
    , parseBool
    , parseUuid
    , parseInt
    , parseIntArr
    , parseStr
    , parseStrArr
    , parseNotNull
    , parseDefault
    , parsePrimaryKey
    , parseUnique
    , parseOnAction
    ) where

import Text.Parsec
import Text.Parsec.Char

parsePsql :: String -> (String,String)
parsePsql str = case runParser parsePsqlExpr "" "Psql params" str of
    Right pair -> pair
    Left error -> ("ERROR PARSING PSQL EXPR","")

parsePsqlExpr,parseType,parseValue,parseRelations,parseConstraint ::
    Parsec String String (String,String)
parsePsqlExpr =
    parseType
    <|> parseValue
    <|> try parseRelations
    <|> parseConstraint
parseType =
    (<*>) (pure (\x -> ("type",x)))
    $ try parseDate
    <|> try parseBool
    <|> try parseUuid
    <|> try parseIntArr
    <|> try parseInt
    <|> try parseStrArr
    <|> try parseStr

parseDate,parseInt, parseIntArr, parseStr, parseStrArr, parseBool, parseUuid ::
    Parsec String String String
parseDate = string "DATE" >> return "date"
parseBool = string "BOOLEAN" >> return "bool"
parseUuid = string "UUID" >> return "uuid"
parseInt = do
    try (string "BIGINT") <|> try (string "INTEGER") <|> string "INT"  <|> string "SMALLINT" <|> string "BIGSERIAL"
    return "int"
parseIntArr = do
    try (string "BIGINT") <|> try (string "INTEGER") <|> string "INT"  <|> string "SMALLINT"
    string "[]"
    return "array int"
parseStr = do
    string "VARCHAR(" <|> string "TEXT"
    return "string"
parseStrArr = do
    string "VARCHAR("
    skipMany digit
    char ')'
    oneOf "[]"
    return "array string"

parseValue =
    (<*>) (pure (\x -> ("value",x)))
    $ try parseNotNull
    <|> try parseDefault

parseNotNull,parseDefault :: Parsec String String String
parseNotNull = string "NOT NULL" >> return "not null"
parseDefault = string "DEFAULT" >> return "null"

parseRelations = do
    string "REFERENCES "
    essence <- many letter
    space
    char '('
    field   <- many letter
    char ')'
    return ("relations","with " <> essence <> " by " <> field)
parseConstraint =
    (<*>) (pure (\x -> ("constraint",x)))
    $ try parsePrimaryKey
    <|> try parseUnique
    <|> try parseOnAction

parsePrimaryKey,parseUnique,parseOnAction :: Parsec String String String
parsePrimaryKey = string "PRIMARY KEY" >> return "primary key"
parseUnique = string "UNIQUE" >> return "unique"
parseOnAction = string "ON DELETE CASCADE" >> return "delete with parent"