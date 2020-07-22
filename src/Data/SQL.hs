module Data.SQL
    ( create
    , edit
    , get
    , delete
    ) where

import Data.List (intercalate)

type Table = String
type Fields = String
type Values = String
type SQLRequest = String
type FieldValue = String

create :: Table -> Fields -> Values -> SQLRequest
create tableName fields values =
    "INSERT INTO " <> tableName
    <> " (" <> fields
    <> ") VALUES (" <> values <> ")"
    <> ";"

edit :: Table -> Values -> Values -> SQLRequest
edit tableName oldValues newValues =
    "UPDATE " <> tableName
    <> " SET " <> newValues
    <> " WHERE " <> oldValues
    <> ";"

get :: Table -> Fields -> FieldValue-> SQLRequest
get tableName fields fieldValue =
    let wherePart =
            if null fieldValue
                then ""
                else " WHERE " <> fieldValue
    in "SELECT " <> fields
        <> " FROM " <> tableName
        <> wherePart
        <> ";"

delete :: Table -> FieldValue -> SQLRequest
delete tableName fieldValue =
    "DELETE FROM " <> tableName
    <> " WHERE " <> fieldValue
    <> ";"