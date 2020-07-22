module Data.SQL.Clause
    ( modifySQLQuery
    , createClause
    , Clause
    ) where

--import Config

import Data.SQL.Actions

import Data.List (interacalate)

type Query = String
type Field = String
type Value = String
type Name  = String

data Clause = Clause
    { name :: Name
    , field :: Field
    , value :: Value
    }
instance Show Clause where
    show (Clause name field value) =
        case name of
            "filter" ->
                (<>) "WHERE " $
                case field of
                    "created_id"    -> "date_of_creation=" <> value
                    "created_after" -> "date_of_creation>" <> value
                    "created_before"-> "date_of_creation<" <> value
                    "tag"           -> value <> "=ANY(tag_ids)"
                    "tags_in"       -> parseTags value
                    "tags_all"      -> "tag_ids=ARRAY" <> value
                    "name"          -> "name=" <> parseSubStr value
                    "content"       -> "content=" <> parseSubStr value
                    "author_name"   ->

            "sort"   ->
                (<>) "ORDER BY " $
                case field of
                    "author_name"      -> parseAuthorName name value
                    "number_of_photos" -> "ARRAY_LENGTH(draft_optional_photos, 1) DESC, draft_main_photo"
                    "category_name"    -> "name"
                    _                  -> field
            "search" ->
                (<>) "WHERE " $
                case field of
                    "author_name" -> parseAuthorName name value
                    _             -> field <> " ILIKE(" <> parseSearchStr value <> ")"

parseTagsIn :: Value -> Query
parseTagsIn value =
    let tagArr = read value :: [Int]
    in interacalate " OR " $ map (\tag -> show tag <> "=ANY(tag_ids)") tagArr

parseAuthorName :: Name -> Value -> Query
parseAuthorName clauseName value =
    let
        fullName = parseFullName value
        firstName = fst fullName
        lastName = snd fullName
    in if null lastName
        then parseAuthorAnyName clauseName firstName
        else parseAuthorFullName clauseName (firstName,lastName)

parseAuthorAnyName :: Name -> Name -> Query
parseAuthorAnyName clauseName name =
    case clauseName of
        "filter" ->
            "first_name ILIKE(" <> name <> ")"
            <> " OR last_name ILIKE(" <> name <> ")"
        "sort"   -> "(" <> firstName <> ")"
        "search" ->
            "first_name ILIKE(" <> parseSearchStr name <> ")"
            <> " OR last_name ILIKE(" <> parseSearchStr name <> ")"

parseAuthorFullName :: Name -> (Name,Name) -> Query
parseAuthorFullName clauseName (firstName,lastName) =
    case clauseName of
        "filter" ->
            "(first_name ILIKE(" <> firstName <> ")"
            <> " AND last_name ILIKE(" <> lastName <> ")"
            <> ") OR ("
            <> "(first_name ILIKE(" <> lastName <> ")"
            <> " AND last_name ILIKE(" <> firstName <> ")"
        "sort"   -> "(" <> firstName <> "," <> lastName <> ")"
        "search" ->
            "(first_name ILIKE(" <> parseSearchStr firstName <> ")"
            <> " AND last_name ILIKE(" <> parseSearchStr lastName <> ")"
            <> ") OR ("
            <> "(first_name ILIKE(" <> parseSearchStr lastName <> ")"
            <> " AND last_name ILIKE(" <> parseSearchStr firstName <> ")"

parseFullName :: Value -> (Name,Name)
parseFullName value =
    let
        firstName = parseStr $ takeWhile (/=' ') value
        lastName = parseStr . tail $ dropWhile (/=' ') value
    in (firstName,lastName)

parseSearchStr :: Value -> Value
parseSearchStr = parseStr . flip (<>) "%" . (:) '%'

parseStr :: Value -> Value
parseStr = flip (<>) "\'" . (:) '\''

parseSubStr :: Value -> Value
parseSubStr = flip (<>) "%\'" . parseStr

modifySQLQuery :: Query -> Clause -> Action
modifySQLQuery action clause =
    init action <> " " <> show clause <> ";"

createClause :: Name -> Field -> Value -> Clause
createClause = Clause