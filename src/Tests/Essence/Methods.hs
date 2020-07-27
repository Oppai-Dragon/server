module Tests.Essence.Parse
    ( essenceParseTests ) where

import Config
import Data.MyValue         (MyValue (..))
import Data.Essence
import Data.Essence.Parse

import Test.HUnit

essenceParseTests =
    [ TestLabel "getEssenceDBTest"              getEssenceDBTest
    , TestLabel "getHashMapDesctiprionTest"     getHashMapDesctiprionTest
    , TestLabel "iterateHashMapDBTest"          iterateHashMapDBTest
    , TestLabel "getEssenceDB'Test"             getEssenceDB'Test
    , TestLabel "parse_edit_ListOfPairsTest"    parse_edit_ListOfPairsTest
    , TestLabel "parse_get_ListOfPairsTest"     parse_get_ListOfPairsTest
    , TestLabel "parse_delete_ListOfPairsTest"  parse_delete_ListOfPairsTest
    , TestLabel "parseOnlyValuesTest"           parseOnlyValuesTest
    , TestLabel "parseOnlyFieldsTest"           parseOnlyFieldsTest
    , TestLabel "withoutEmptyTest"              withoutEmptyTest
    , TestLabel "toListTest"                    toListTest
    , TestLabel "parse_Just_BSValueTest"        parse_Just_BSValueTest
    , TestLabel "parse_Nothing_BSValueTest"     parse_Nothing_BSValueTest
    , TestLabel "parseFieldValueTest"           parseFieldValueTest
    , TestLabel "toEssenceListTest"             toEssenceListTest
    ]

getEssenceDBTest =
    TestCase $
    assertEqual "for (getEssenceDB )"

    $ getEssenceDB

getHashMapDesctiprionTest =
    TestCase $
    assertEqual "for (getHashMapDesctiprion )"

    $ getHashMapDesctiprion

iterateHashMapDBTest =
    TestCase $
    assertEqual "for (iterateHashMapDB )"

    $ iterateHashMapDB

getEssenceDB'Test =
    TestCase $
    assertEqual "for (getEssenceDB' )"

    $ getEssenceDB' "person" testConfig

parse_edit_ListOfPairsTest =
    TestCase $
    assertEqual "for (parseListOfPairs \"edit\" [(\"field1\",\"value1\"), (\"field2\",\"value2\")])"
    "field1=value1,field2=value2"
    $ parseListOfPairs "edit" [("field1","value1"), ("field2","value2")]

parse_get_ListOfPairsTest =
    TestCase $
    assertEqual "for (parseListOfPairs \"get\" [(\"field1\",\"value1\"), (\"field2\",\"value2\")])"
    "field1=value1 AND field2=value2"
    $ parseListOfPairs "get" [("field1","value1"), ("field2","value2")]

parse_delete_ListOfPairsTest =
    TestCase $
    assertEqual "for (parseListOfPairs \"delete\" [(\"field1\",\"value1\"), (\"field2\",\"value2\")])"
    "field1=value1 AND field2=value2"
    $ parseListOfPairs "delete" [("field1","value1"), ("field2","value2")]

parseOnlyValuesTest =
    TestCase $
    assertEqual "for (parseOnlyValues [(\"field\",\"value\")])"
    "value"
    $ parseOnlyValues [("field","value")]

parseOnlyFieldsTest =
    TestCase $
    assertEqual "for (parseOnlyFields [(\"field\",\"values\")])"
    "field"
    $ parseOnlyFields [("field","values")]

withoutEmptyTest =
    TestCase $
    assertEqual "for (withoutEmpty [(\"field\", MyEmpty)])"
    []
    $ withoutEmpty [("field", MyEmpty)]

toListTest =
    TestCase $
    assertEqual "for (toList [\"field\", MyString \"value\"])"
    [("field", "'value'")]
    $ toList [("field", MyString "value")]

parse_Just_BSValueTest =
    TestCase $
    assertEqual "for (parseBSValue \"id\" [(\"id\", Just \"1\")])"
    ("id", MyInteger 1)
    $ parseBSValue "id" [("id", Just "1")]

parse_Nothing_BSValueTest =
    TestCase $
    assertEqual "for (parseBSValue \"id\" [(\"id\", Nothing)])"
    ("id", MyEmpty)
    $ parseBSValue "id" [("id", Nothing)]

parseFieldValueTest =
    TestCase $
    assertEqual "for (parseFieldValue [(\"id\", Just \"1\"), (\"first_name\", Nothing)])"
    [("id", MyInteger 1), ("first_name", MyEmpty)]
    $ parseFieldValue ["id","first_name"] [("id", Just "1"), ("first_name", Nothing)]

fromQueryTest =
    TestCase $
    assertEqual "for (fromQuery \"user\" [(\"id\", Just \"1\")] testConfig)"
    (Essence "category" [("id", "1")])
    $ fromQuery "category" [("id", Just "1")] testConfig