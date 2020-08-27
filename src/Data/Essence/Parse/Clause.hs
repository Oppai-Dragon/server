module Data.Essence.Parse.Clause
  ( toEssenceClause
  , pickTableName
  , pickClause
  , matchEssence
  , addEssenceName
  , parseTagsIn
  , parseAuthorName
  , parseAuthorAnyName
  , parseAuthorFullName
  , parseFullName
  , parseSearchStr
  , parseStr
  , parseSubStr
  ) where

import Data.Base
import Data.Empty
import Data.Essence
import Data.MyValue
import Data.SQL

import Data.List

import Control.Monad.Trans.Writer.CPS

type EssenceName = String

type Field = String

toEssenceClause :: Essence List -> Writer (Essence (Clause String)) ()
toEssenceClause (EssenceList _ _ []) = return ()
toEssenceClause (EssenceList name _ ((field, myValue):rest)) =
  let tableList = pickTableName field
      clauseArr = pickClause name (field, myValue)
   in tell (EssenceClause tableList clauseArr) >>
      toEssenceClause (EssenceList name "" rest)

pickTableName :: Field -> [EssenceName]
pickTableName field =
  let clause = takeWhile (/= '_') field
      specificField = tailCase $ dropWhile (/= '_') field
   in case clause of
        "filter" ->
          case specificField of
            "author_name" -> ["author", "person"]
            _ -> []
        "search" ->
          case specificField of
            "author_name" -> ["author", "person"]
            "category_name" -> ["category"]
            "tag_name" -> ["tag"]
            _ -> []
        "sort" ->
          case specificField of
            "author_name" -> ["author", "person"]
            "category_name" -> ["category"]
            _ -> []
        _ -> []

pickClause :: EssenceName -> (Field, MyValue) -> [Clause String]
pickClause name (field, myValue) =
  let clause = takeWhile (/= '_') field
      specificField = tailCase $ dropWhile (/= '_') field
      value = parseValue myValue
      valueStr = toStr myValue
      offsetLimit =
        case myValue of
          MyString x -> x
          _ -> ""
   in case clause of
        "filter" ->
          flip (:) [] $
          case specificField of
            "created_in" -> Where (name +. "date_of_creation", myValue)
            "created_after" -> Filter $ name +. "date_of_creation>" <> value
            "created_before" -> Filter $ name +. "date_of_creation<" <> value
            "category_id" -> Where (name +. "category_id", myValue)
            "tag_id" -> Filter $ value <> "=ANY(" <> name +. "tag_ids)"
            "tags_in" -> Filter $ parseTagsIn valueStr
            "tags_all" -> Where (name +. "tag_ids", myValue)
            "name" ->
              Filter $
              name +. "name ILIKE " <> (parseStr . parseSubStr) valueStr
            "content" ->
              Filter $
              name +. "content ILIKE " <> (parseStr . parseSubStr) valueStr
            "author_name" -> Filter $ parseAuthorName clause valueStr
            _ -> Where ("", MyEmpty)
        "search" ->
          flip (:) [] $
          case specificField of
            "author_name" -> Filter $ parseAuthorName clause valueStr
            "content" ->
              Filter $
              name +. "content ILIKE " <> (parseStr . parseSearchStr) valueStr
            "category_name" ->
              Filter $
              "category.name ILIKE " <> (parseStr . parseSearchStr) valueStr
            "tag_name" ->
              Filter $ "tag.name ILIKE " <> (parseStr . parseSearchStr) valueStr
            _ -> Where ("", MyEmpty)
        "sort" ->
          case myValue of
            MyBool True ->
              case specificField of
                "author_name" ->
                  [OrderBy "person.last_name", OrderBy "person.first_name"]
                "number_of_photos" ->
                  [ OrderBy
                      ("ARRAY_LENGTH(" <>
                       name +. "draft_optional_photos, 1) DESC")
                  , OrderBy (name +. "draft_main_photo")
                  ]
                "category_name" -> [OrderBy "category.name"]
                "date_of_creation" -> [OrderBy $ name +. "date_of_creation"]
                _ -> []
            _ -> []
        "page" -> [OffsetLimit offsetLimit]
        _ -> [Where (name +. field, myValue)]

matchEssence :: [EssenceName] -> [Clause String]
matchEssence [] = []
matchEssence (table:rest) =
  let parsed =
        case table of
          "author" -> Filter "news.author_id=author.id"
          "person" -> Filter "author.person_id=person.id"
          "category" -> Filter "news.category_id=category.id"
          "tag" -> Filter "tag.id=ANY(news.tag_ids)"
          _ -> Filter ""
   in case parsed of
        Filter "" -> matchEssence rest
        _ -> parsed : matchEssence rest

addEssenceName, (+.) :: EssenceName -> Field -> Field
(+.) = addEssenceName

addEssenceName name field = name <> "." <> field

parseTagsIn :: String -> String
parseTagsIn value =
  let tagArr = read value :: [Int]
   in intercalate " OR " $ map (\tag -> show tag <> "=ANY(tag_ids)") tagArr

parseAuthorName :: String -> String -> String
parseAuthorName clauseName value =
  let fullName = parseFullName value
      firstName = fst fullName
      lastName = snd fullName
   in if null lastName
        then parseAuthorAnyName clauseName firstName
        else parseAuthorFullName clauseName (firstName, lastName)

parseAuthorAnyName :: String -> String -> String
parseAuthorAnyName clauseName name =
  let nameStr = parseStr name
      searchStr = parseStr $ parseSearchStr name
   in case clauseName of
        "filter" ->
          "person.first_name ILIKE " <>
          nameStr <> " OR person.last_name ILIKE " <> nameStr
        "search" ->
          "person.first_name ILIKE " <>
          searchStr <> " OR person.last_name ILIKE " <> searchStr
        _ -> ""

parseAuthorFullName :: String -> (String, String) -> String
parseAuthorFullName clauseName (firstName, lastName) =
  let f = parseStr firstName
      l = parseStr lastName
      searchF = parseStr $ parseSearchStr firstName
      searchL = parseStr $ parseSearchStr lastName
   in case clauseName of
        "filter" ->
          "(person.first_name ILIKE " <>
          f <>
          " AND person.last_name ILIKE " <>
          l <>
          ") OR (" <>
          "person.first_name ILIKE " <>
          l <> " AND person.last_name ILIKE " <> f <> ")"
        "search" ->
          "(person.first_name ILIKE " <>
          searchF <>
          " AND person.last_name ILIKE " <>
          searchL <>
          ") OR (" <>
          "person.first_name ILIKE " <>
          searchL <> " AND person.last_name ILIKE " <> searchF <> ")"
        _ -> ""

parseFullName :: String -> (String, String)
parseFullName value =
  let firstName = takeWhile (/= ' ') value
      lastName = tailCase $ dropWhile (/= ' ') value
   in (firstName, lastName)

parseSearchStr :: String -> String
parseSearchStr = flip (<>) "%" . (:) '%'

parseStr :: String -> String
parseStr = flip (<>) "\'" . (:) '\''

parseSubStr :: String -> String
parseSubStr = flip (<>) "%"
