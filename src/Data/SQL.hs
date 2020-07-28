module Data.SQL
    ( create
    , edit
    , get
    , delete
    ) where

import Data.MyValue

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

data SqlQuery
    = Insert
        { insertTable   :: String
        , insertFields  :: [String]
        , insertValues  :: [MyValue]
        }
    | Edit
        { editTable :: String
        , editSet   :: [(String,MyValue)]
        , editWhere :: [Clause]
        }
    | Get
        { getTable   :: String
        , getClauses :: [Clause]
        }
    | Delete
        { deleteTable :: String
        , deleteWhere :: [Clause]
        }
--instance Show SqlQuery where
--    show (Insert table fields values) =
--        "INSERT INTO " <> table
--        <> " " <> parseFields fields
--        <> " VALUES " <> parseValues values
--        <> ";"
--    show (Edit table fieldsValues wherePart) =
--        "UPDATE " <> table
--        <> " SET " <> parseFiedlsValues fieldsValues
--        <> " " <> show wherePart
--        <> ";"
--    show (Get table clauses) =
--        "SELECT * FROM " <> table
--        <> " " <> (show . sort) clauses
--        <> ";"
--    show (Delete table wherePart) =
--        "DELETE FROM " <> table
--        <> " " <> show clauses
--        <> ";"
--
data Clause
    = Clause
        { nameClause  :: String
        , fieldClause :: String
        , valueClause :: MyValue
        }
    | Where
        { fieldWhere  :: String
        , valueWhere  :: String
        }
    | OrderBy
        { fieldsOrderBy :: [String]
        }
--instance Show Clause where
--    show (Clause name field myValue) = undefined
--        --let value = forPsql myValue
--        --in case name of
--        --    "filter" ->
--        --        Where $
--        --        case field of
--        --            "created_id"    -> undefined undefined -- "date_of_creation=" value
--        --            "created_after" -> undefined undefined -- "date_of_creation>" value
--        --            "created_before"-> undefined undefined -- "date_of_creation<" value
--        --            "tag"           -> undefined undefined -- value "=ANY(tag_ids)"
--        --            "tags_in"       -> undefined undefined -- parseTagsIn value
--        --            "tags_all"      -> undefined undefined -- "tag_ids=ARRAY"  value
--        --            "name"          -> undefined undefined -- "name=" (parseSubStr value)
--        --            "content"       -> undefined undefined -- "content=" (parseSubStr value)
--        --            "author_name"   -> undefined undefined -- parseAuthorName name value
--        --
--        --    "sort"   ->
--        --        OrderBy $
--        --        case field of
--        --            "author_name"      -> undefined --parseAuthorName name value
--        --            "number_of_photos" -> undefined --"ARRAY_LENGTH(draft_optional_photos, 1) DESC, draft_main_photo"
--        --            "category_name"    -> undefined --"name"
--        --            _                  -> undefined --field
--        --    "search" ->
--        --        Where $
--        --        case field of
--        --            "author_name" -> undefined undefined --parseAuthorName name value
--        --            _             -> undefined undefined --field (" ILIKE(" <> parseSearchStr value <> ")")
--
--parseTagsIn :: String -> String
--parseTagsIn value =
--    let tagArr = read value :: [Int]
--    in intercalate " OR " $ map (\tag -> show tag <> "=ANY(tag_ids)") tagArr
--
--parseAuthorName :: String -> String -> String
--parseAuthorName clauseName value =
--    let
--        fullName = parseFullName value
--        firstName = fst fullName
--        lastName = snd fullName
--    in if null lastName
--        then parseAuthorAnyName clauseName firstName
--        else parseAuthorFullName clauseName (firstName,lastName)
--
--parseAuthorAnyName :: String -> String -> String
--parseAuthorAnyName clauseName name =
--    case clauseName of
--        "filter" ->
--            "first_name ILIKE(" <> name <> ")"
--            <> " OR last_name ILIKE(" <> name <> ")"
--        "sort"   -> "(" <> clauseName <> ")"
--        "search" ->
--            "first_name ILIKE(" <> parseSearchStr name <> ")"
--            <> " OR last_name ILIKE(" <> parseSearchStr name <> ")"
--
--parseAuthorFullName :: String -> (String,String) -> String
--parseAuthorFullName clauseName (firstName,lastName) =
--    case clauseName of
--        "filter" ->
--            "(first_name ILIKE(" <> firstName <> ")"
--            <> " AND last_name ILIKE(" <> lastName <> ")"
--            <> ") OR ("
--            <> "(first_name ILIKE(" <> lastName <> ")"
--            <> " AND last_name ILIKE(" <> firstName <> ")"
--        "sort"   -> "(" <> firstName <> "," <> lastName <> ")"
--        "search" ->
--            "(first_name ILIKE(" <> parseSearchStr firstName <> ")"
--            <> " AND last_name ILIKE(" <> parseSearchStr lastName <> ")"
--            <> ") OR ("
--            <> "(first_name ILIKE(" <> parseSearchStr lastName <> ")"
--            <> " AND last_name ILIKE(" <> parseSearchStr firstName <> ")"
--
--parseFullName :: String -> (String,String)
--parseFullName value =
--    let
--        firstName = parseStr $ takeWhile (/=' ') value
--        lastName = parseStr . tail $ dropWhile (/=' ') value
--    in (firstName,lastName)
--
--parseSearchStr :: String -> String
--parseSearchStr = parseStr . flip (<>) "%" . (:) '%'
--
--parseStr :: String -> String
--parseStr = flip (<>) "\'" . (:) '\''
--
--parseSubStr :: String -> String
--parseSubStr = flip (<>) "%\'" . parseStr
--
--modifySQLQuery :: String -> Clause -> String
--modifySQLQuery action clause = init action <> " " <> show clause <> ";"
--
--createClause :: String -> String -> MyValue -> Clause
--createClause = Clause