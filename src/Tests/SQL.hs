module Tests.SQL
    ( sqlTests
    ) where

import Data.MyValue
import Data.SQL

import Test.HUnit

sqlTests =
    [ TestLabel "showSql_insert_Test"               showSql_insert_Test
    , TestLabel "showSql_edit_Test"                 showSql_edit_Test
    , TestLabel "showSql_get_Test"                  showSql_get_Test
    , TestLabel "showSql_delete_Test"               showSql_delete_Test
    , TestLabel "showSql_arrClauseString_Test"      showSql_arrClauseString_Test
    , TestLabel "showSql_setList_Test"              showSql_setList_Test
    , TestLabel "showSql_whereList_Test"            showSql_whereList_Test
    , TestLabel "showSql_filterList_Test"           showSql_filterList_Test
    , TestLabel "showSql_orderByList_Test"          showSql_orderByList_Test
    , TestLabel "showSql_arrClauseArrString_Test"   showSql_arrClauseArrString_Test
    , TestLabel "clauseSequenceATest"               clauseSequenceATest
    , TestLabel "withoutManyWhereTest"              withoutManyWhereTest
    ]

showSql_insert_Test =
    TestCase $
    assertEqual
    "for(showSql (Insert \"person\" [\"first_name\"] [MyString \"misha\"]))"
    "INSERT INTO person (first_name) VALUES ('misha');"
    $ showSql (Insert "person" ["first_name"] [MyString "misha"])
showSql_edit_Test =
    TestCase $
    assertEqual
    ("for(showSql "
    <> "(Edit \"person\""
    <> "[Set (\"first_name\",MyString \"misha\"),Set (\"last_name\",MyString \"dragon\")]"
    <> "[Where (\"id\",MyInteger 1)]"
    <> ")")
    "UPDATE person SET first_name='misha',last_name='dragon' WHERE id=1;"
    $ showSql (Edit "person"
    [Set ("first_name",MyString "misha"),Set ("last_name",MyString "dragon")]
    [Where ("id",MyInteger 1)])
showSql_get_Test =
    TestCase $
    assertEqual
    ("for(showSql (Get \"person\" "
    <> "[Where (\"first_name\",MyString \"misha\")"
    <> ",Filter \"last_name ILIKE '%dragon%'\""
    <> ",OrderBy \"date_of_creation\""
    <> "]))")
    ("SELECT * FROM person "
    <> "WHERE first_name='misha' AND last_name ILIKE '%dragon%' ORDER BY (date_of_creation);"
    )
    $ showSql (Get "person"
        [Where ("first_name",MyString "misha")
        ,Filter "last_name ILIKE '%dragon%'"
        ,OrderBy "date_of_creation"
        ])
showSql_delete_Test =
    TestCase $
    assertEqual
    "for(showSql (Delete \"person\" [Where (\"id\",MyInteger 1)]))"
    "DELETE FROM person WHERE id=1;"
    $ showSql (Delete "person" [Where ("id",MyInteger 1)])
showSql_arrClauseString_Test =
    TestCase $
    assertEqual
    ("for(showSql "
    <> "[Where (\"first_name\",\"'misha'\")"
    <> ",Filter \"last_name ILIKE ('%dragon%')\""
    <> ",OrderBy \"date_of_creation\""
    <> "])")
    "WHERE first_name='misha' AND last_name ILIKE '%dragon%' ORDER BY (date_of_creation)"
    $ showSql
    [Where ("first_name",MyString "misha")
    ,Filter "last_name ILIKE '%dragon%'"
    ,OrderBy "date_of_creation"
    ]
showSql_setList_Test =
    TestCase $
    assertEqual "for(showSql (SetList [(\"first_name\",\"'misha'\")]))"
    " SET first_name='misha'"
    $ showSql (SetList [("first_name","'misha'")])
showSql_whereList_Test =
    TestCase $
    assertEqual
    "for(showSql (WhereList [(\"first_name\",\"'misha'\")]))"
    " WHERE first_name='misha'"
    $ showSql (WhereList [("first_name","'misha'")])
showSql_filterList_Test =
    TestCase $
    assertEqual
    "for(showSql (FilterList [\"last_name ILIKE ('%dragon%')\"]))"
    " WHERE last_name ILIKE '%dragon%'"
    $ showSql (FilterList ["last_name ILIKE '%dragon%'"])
showSql_orderByList_Test =
    TestCase $
    assertEqual
    "for(showSql (OrderByList [\"date_of_creation\"]))"
    " ORDER BY (date_of_creation)"
    $ showSql (OrderByList ["date_of_creation"])
showSql_arrClauseArrString_Test =
    TestCase $
    assertEqual
    ("for(showSql "
    <> "[WhereList [(\"first_name\",\"'misha'\")]"
    <> ",FilterList [\"last_name ILIKE ('%dragon%')\"]"
    <> ",OrderByList [\"date_of_creation\"]"
    <> "])")
    "WHERE first_name='misha' AND last_name ILIKE '%dragon%' ORDER BY (date_of_creation)"
    $ showSql
    [WhereList [("first_name","'misha'")]
    ,FilterList ["last_name ILIKE '%dragon%'"]
    ,OrderByList ["date_of_creation"]
    ]

clauseSequenceATest =
    TestCase $
    assertEqual
    "for(clauseSequenceA [Where (\"first_name\",MyString \"misha\"),Where (\"last_name\",MyString \"dragon\")])"
    (WhereList [("first_name","'misha'"),("last_name","'dragon'")])
    $ clauseSequenceA
    [Where ("first_name",MyString "misha"),Where ("last_name",MyString "dragon")]

withoutManyWhereTest =
    TestCase $
    assertEqual
    "for (withoutManyWhere [\"WHERE\",\"first_name\",\"WHERE\",\"last_name\"])"
    ["AND","first_name","WHERE","last_name"]
    $ withoutManyWhere ["WHERE","first_name","WHERE","last_name"]