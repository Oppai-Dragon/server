{-# LANGUAGE TupleSections #-}

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

parsePsql :: String -> (String, String)
parsePsql str =
  case runParser parsePsqlExpr "" "Psql params" str of
    Right pair -> pair
    Left _ -> ("ERROR PARSING PSQL EXPR", "")

parsePsqlExpr, parseType, parseValue, parseRelations, parseConstraint ::
     Parsec String String (String, String)
parsePsqlExpr =
  parseType <|> parseValue <|> try parseRelations <|> parseConstraint

parseType =
  fmap ("type", ) $ try parseDate <|> try parseBool <|> try parseUuid <|>
  try parseIntArr <|>
  try parseInt <|>
  try parseStrArr <|>
  try parseStr

parseDate, parseInt, parseIntArr, parseStr, parseStrArr, parseBool, parseUuid ::
     Parsec String String String
parseDate = string "DATE" >> return "date"

parseBool = string "BOOLEAN" >> return "bool"

parseUuid = string "UUID" >> return "uuid"

parseInt =
  (try (string "BIGINT") <|> try (string "INTEGER") <|> string "INT" <|>
   string "SMALLINT" <|>
   string "BIGSERIAL") >>
  return "int"

parseIntArr =
  (try (string "BIGINT") <|> try (string "INTEGER") <|> string "INT" <|>
   string "SMALLINT") >>
  string "[]" >>
  return "array int"

parseStr = (string "VARCHAR(" <|> string "TEXT") >> return "string"

parseStrArr =
  string "VARCHAR(" >> skipMany digit >> char ')' >> oneOf "[]" >>
  return "array string"

parseValue = fmap ("value", ) $ try parseNotNull <|> try parseDefault

parseNotNull, parseDefault :: Parsec String String String
parseNotNull = string "NOT NULL" >> return "not null"

parseDefault = string "DEFAULT" >> return "null"

parseRelations = do
  _ <- string "REFERENCES "
  essence <- many letter
  _ <- space
  _ <- char '('
  field <- many letter
  _ <- char ')'
  return ("relations", "with " <> essence <> " by " <> field)

parseConstraint =
  fmap ("constraint", ) $ try parsePrimaryKey <|> try parseUnique <|>
  try parseOnAction

parsePrimaryKey, parseUnique, parseOnAction :: Parsec String String String
parsePrimaryKey = string "PRIMARY KEY" >> return "primary key"

parseUnique = string "UNIQUE" >> return "unique"

parseOnAction = return ""
