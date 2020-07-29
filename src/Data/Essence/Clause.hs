module Data.Essence.Clause
    ( parseTagsIn
    , parseAuthorName
    , parseAuthorAnyName
    , parseAuthorFullName
    , parseFullName
    , parseSearchStr
    , parseStr
    , parseSubStr
    ) where

import Data.List

parseTagsIn :: String -> String
parseTagsIn value =
    let tagArr = read value :: [Int]
    in intercalate " OR " $ map (\tag -> show tag <> "=ANY(tag_ids)") tagArr

parseAuthorName :: String -> String -> String
parseAuthorName clauseName value =
    let
        fullName = parseFullName value
        firstName = fst fullName
        lastName = snd fullName
    in if null lastName
        then parseAuthorAnyName clauseName firstName
        else parseAuthorFullName clauseName (firstName,lastName)

parseAuthorAnyName :: String -> String -> String
parseAuthorAnyName clauseName name =
    let
        nameStr = parseStr name
        searchStr = parseSearchStr name
    in case clauseName of
        "filter" ->
            "first_name ILIKE(" <> nameStr <> ")"
            <> " OR last_name ILIKE(" <> nameStr <> ")"
        "search" ->
            "first_name ILIKE(" <> searchStr <> ")"
            <> " OR last_name ILIKE(" <> searchStr <> ")"

parseAuthorFullName :: String -> (String,String) -> String
parseAuthorFullName clauseName (firstName,lastName) =
    let
        f = parseStr firstName
        l = parseStr lastName
        searchF = parseSearchStr firstName
        searchL = parseSearchStr lastName
    in case clauseName of
        "filter" ->
            "(first_name ILIKE(" <> f <> ")"
            <> " AND last_name ILIKE(" <> l <> ")"
            <> ") OR ("
            <> "first_name ILIKE(" <> l <> ")"
            <> " AND last_name ILIKE(" <> f <> "))"
        "search" ->
            "(first_name ILIKE(" <> searchF <> ")"
            <> " AND last_name ILIKE(" <> searchL <> ")"
            <> ") OR ("
            <> "(first_name ILIKE(" <> searchL <> ")"
            <> " AND last_name ILIKE(" <> searchF <> ")"

parseFullName :: String -> (String,String)
parseFullName value =
    let
        firstName = takeWhile (/=' ') value
        lastName = tail $ dropWhile (/=' ') value
    in (firstName,lastName)

parseSearchStr :: String -> String
parseSearchStr = parseStr . flip (<>) "%" . (:) '%'

parseStr :: String -> String
parseStr = flip (<>) "\'" . (:) '\''

parseSubStr :: String -> String
parseSubStr = parseStr . flip (<>) "%"