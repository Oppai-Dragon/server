module Tests.SQL.Actions
    ( sqlActionsTests
    ) where

import Data.Essence
import Data.SQL
import Data.SQL.Actions

import Tests.Essence

import Test.HUnit

sqlActionsTests =
    [ TestLabel "showSql_create_Test"   showSql_create_Test
    , TestLabel "showSql_edit_Test"     showSql_edit_Test
    , TestLabel "showSql_get_Test"      showSql_get_Test
    , TestLabel "showSql_delete_Test"   showSql_delete_Test
    ]

showSql_create_Test =
    TestCase $
    assertEqual
    "for (showSql (EssenceList \"news\" \"create\" testNewsCreateFields))"
    "INSERT INTO news (id,content) VALUES (1,'kek');"
    $ showSql (EssenceList "news" "create" testNewsCreateFields)
showSql_edit_Test =
    TestCase $
    assertEqual
    "for (showS (EssenceList \"news\" \"edit\" testNewsEditFields))"
    "UPDATE news SET id=1,content='kek' WHERE id=1;"
    $ showSql (EssenceList "news" "edit" testNewsEditFields)
showSql_get_Test =
    TestCase $
    assertEqual
    "for (show (EssenceList \"news\" \"get\" testNewsGetFields))"
    ("SELECT * FROM news,author,person,category "
    <> "WHERE news.id=1 AND news.author_id=author.id AND author.person_id=person.id "
    <> "AND news.category_id=category.id "
    <> "AND (person.first_name ILIKE 'misha' AND person.last_name ILIKE 'dragon') "
    <> "OR (person.first_name ILIKE 'dragon' AND person.last_name ILIKE 'misha') "
    <> "AND category.name ILIKE '%cat%' ORDER BY (news.date_of_creation);")
    $ showSql (EssenceList "news" "get" testNewsGetFields)
showSql_delete_Test =
    TestCase $
    assertEqual
    "for (showSql (EssenceList \"news\" \"delete\" testNewsDeleteFields))"
    "DELETE FROM news WHERE id=1;"
    $ showSql (EssenceList "news" "delete" testNewsDeleteFields)