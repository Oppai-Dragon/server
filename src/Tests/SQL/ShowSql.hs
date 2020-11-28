module Tests.SQL.ShowSql
  ( showSqlTests
  ) where

import Data.Essence
import Data.MyValue
import Data.SQL
import Data.SQL.ShowSql

import Tests.Essence

import Test.HUnit

showSqlTests :: [Test]
showSqlTests =
  [ TestLabel "showSqlInsertTest" showSqlInsertTest
  , TestLabel "showSqlEditTest" showSqlEditTest
  , TestLabel "showSqlGetTest" showSqlGetTest
  , TestLabel "showSqlDeleteTest" showSqlDeleteTest
  , TestLabel "showSqlArrClauseStringTest" showSqlArrClauseStringTest
  , TestLabel "showSqlSetListTest" showSqlSetListTest
  , TestLabel "showSqlWhereListTest" showSqlWhereListTest
  , TestLabel "showSqlFilterListTest" showSqlFilterListTest
  , TestLabel "showSqlOrderByListTest" showSqlOrderByListTest
  , TestLabel "showSqlArrClauseArrStringTest" showSqlArrClauseArrStringTest
  , TestLabel "clauseSequenceATest" clauseSequenceATest
  , TestLabel "withoutManyWhereTest" withoutManyWhereTest
  ] <>
  sqlActionsTests

showSqlInsertTest, showSqlEditTest, showSqlGetTest, showSqlDeleteTest, showSqlArrClauseStringTest, showSqlSetListTest, showSqlWhereListTest, showSqlFilterListTest, showSqlOrderByListTest, showSqlArrClauseArrStringTest, clauseSequenceATest, withoutManyWhereTest ::
     Test
showSqlInsertTest =
  TestCase $
  assertEqual
    "for(showSql (Insert \"person\" [\"first_name\"] [MyString \"misha\"]))"
    "INSERT INTO person (first_name) VALUES ('misha');" $
  showSql (Insert "person" ["first_name"] [MyString "misha"])

showSqlEditTest =
  TestCase $
  assertEqual
    ("for(showSql " <>
     "(Edit \"person\"" <>
     "[Set (\"first_name\",MyString \"misha\"),Set (\"last_name\",MyString \"dragon\")]" <>
     "[Where (\"id\",MyInteger 1)]" <> ")")
    "UPDATE person SET first_name='misha',last_name='dragon' WHERE id=1;" $
  showSql
    (Edit
       "person"
       [ Set ("first_name", MyString "misha")
       , Set ("last_name", MyString "dragon")
       ]
       [Where ("id", MyInteger 1)])

showSqlGetTest =
  TestCase $
  assertEqual
    ("for(showSql (Get \"person\" " <>
     "[Where (\"first_name\",MyString \"misha\")" <>
     ",Filter \"last_name ILIKE '%dragon%'\"" <>
     ",OrderBy \"date_of_creation\"" <> "]))")
    ("SELECT * FROM person " <>
     "WHERE first_name='misha' AND last_name ILIKE '%dragon%' ORDER BY (date_of_creation);") $
  showSql
    (Get
       "person"
       [ Where ("first_name", MyString "misha")
       , Filter "last_name ILIKE '%dragon%'"
       , OrderBy "date_of_creation"
       ])

showSqlDeleteTest =
  TestCase $
  assertEqual
    "for(showSql (Delete \"person\" [Where (\"id\",MyInteger 1)]))"
    "DELETE FROM person WHERE id=1;" $
  showSql (Delete "person" [Where ("id", MyInteger 1)])

showSqlArrClauseStringTest =
  TestCase $
  assertEqual
    ("for(showSql " <>
     "[Where (\"first_name\",\"'misha'\")" <>
     ",Filter \"last_name ILIKE ('%dragon%')\"" <>
     ",OrderBy \"date_of_creation\"" <> "])")
    "WHERE first_name='misha' AND last_name ILIKE '%dragon%' ORDER BY (date_of_creation)" $
  showSql
    [ Where ("first_name", MyString "misha")
    , Filter "last_name ILIKE '%dragon%'"
    , OrderBy "date_of_creation"
    ]

showSqlSetListTest =
  TestCase $
  assertEqual
    "for(showSql (SetList [(\"first_name\",\"'misha'\")]))"
    " SET first_name='misha'" $
  showSql (SetList [("first_name", "'misha'")])

showSqlWhereListTest =
  TestCase $
  assertEqual
    "for(showSql (WhereList [(\"first_name\",\"'misha'\")]))"
    " WHERE first_name='misha'" $
  showSql (WhereList [("first_name", "'misha'")])

showSqlFilterListTest =
  TestCase $
  assertEqual
    "for(showSql (FilterList [\"last_name ILIKE ('%dragon%')\"]))"
    " WHERE last_name ILIKE '%dragon%'" $
  showSql (FilterList ["last_name ILIKE '%dragon%'"])

showSqlOrderByListTest =
  TestCase $
  assertEqual
    "for(showSql (OrderByList [\"date_of_creation\"]))"
    " ORDER BY (date_of_creation)" $
  showSql (OrderByList ["date_of_creation"])

showSqlArrClauseArrStringTest =
  TestCase $
  assertEqual
    ("for(showSql " <>
     "[WhereList [(\"first_name\",\"'misha'\")]" <>
     ",FilterList [\"last_name ILIKE ('%dragon%')\"]" <>
     ",OrderByList [\"date_of_creation\"]" <> "])")
    "WHERE first_name='misha' AND last_name ILIKE '%dragon%' ORDER BY (date_of_creation)" $
  showSql
    [ WhereList [("first_name", "'misha'")]
    , FilterList ["last_name ILIKE '%dragon%'"]
    , OrderByList ["date_of_creation"]
    ]

sqlActionsTests :: [Test]
sqlActionsTests =
  [ TestLabel "showSqlEssenceCreateTest" showSqlEssenceCreateTest
  , TestLabel "showSqlEssenceEditTest" showSqlEssenceEditTest
  , TestLabel "showSqlEssenceGetTest" showSqlEssenceGetTest
  , TestLabel "showSqlEssenceDeleteTest" showSqlEssenceDeleteTest
  ]

showSqlEssenceCreateTest, showSqlEssenceEditTest, showSqlEssenceGetTest, showSqlEssenceDeleteTest ::
     Test
showSqlEssenceCreateTest =
  TestCase $
  assertEqual
    "for (showSql (EssenceList \"news\" \"create\" testNewsCreateFields))"
    "INSERT INTO news (id,content) VALUES (1,'kek');" $
  showSql
    EssenceList
      {elName = "news", elAction = "create", elList = testNewsCreateFields}

showSqlEssenceEditTest =
  TestCase $
  assertEqual
    "for (showS (EssenceList \"news\" \"edit\" testNewsEditFields))"
    "UPDATE news SET id=1,content='kek' WHERE id=1;" $
  showSql
    EssenceList
      {elName = "news", elAction = "edit", elList = testNewsEditFields}

showSqlEssenceGetTest =
  TestCase $
  assertEqual
    "for (show (EssenceList \"news\" \"get\" testNewsGetFields))"
    ("SELECT * FROM news,author,person,category " <>
     "WHERE news.id=1 AND news.author_id=author.id AND author.person_id=person.id " <>
     "AND news.category_id=category.id " <>
     "AND (person.first_name ILIKE 'misha' AND person.last_name ILIKE 'dragon') " <>
     "OR (person.first_name ILIKE 'dragon' AND person.last_name ILIKE 'misha') " <>
     "AND category.name ILIKE '%cat%' ORDER BY (news.date_of_creation);") $
  showSql
    EssenceList {elName = "news", elAction = "get", elList = testNewsGetFields}

showSqlEssenceDeleteTest =
  TestCase $
  assertEqual
    "for (showSql (EssenceList \"news\" \"delete\" testNewsDeleteFields))"
    "DELETE FROM news WHERE id=1;" $
  showSql
    EssenceList
      {elName = "news", elAction = "delete", elList = testNewsDeleteFields}

clauseSequenceATest =
  TestCase $
  assertEqual
    "for(clauseSequenceA [Where (\"first_name\",MyString \"misha\"),Where (\"last_name\",MyString \"dragon\")])"
    (WhereList [("first_name", "'misha'"), ("last_name", "'dragon'")]) $
  clauseSequenceA
    [ Where ("first_name", MyString "misha")
    , Where ("last_name", MyString "dragon")
    ]

withoutManyWhereTest =
  TestCase $
  assertEqual
    "for (withoutManyWhere [\"WHERE\",\"first_name\",\"WHERE\",\"last_name\"])"
    ["AND", "first_name", "WHERE", "last_name"] $
  withoutManyWhere ["WHERE", "first_name", "WHERE", "last_name"]
