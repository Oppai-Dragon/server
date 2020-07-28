module Tests.Essence.Methods
    ( essenceMethodsTests
    ) where

import Config
import Data.MyValue         (MyValue (..))
import Data.Essence
import Data.Essence.Methods
import Data.SQL.Actions

import qualified Data.HashMap.Strict as HM
import           Control.Monad.Trans.Reader

import Tests.Essence

import Test.HUnit

essenceMethodsTests =
    [ TestLabel "addListTest"                   addListTest
    , TestLabel "deletePairTest"                deletePairTest
    , TestLabel "getEssenceDBTest"              getEssenceDBTest
    , TestLabel "getEssenceDB'Test"             getEssenceDB'Test
    , TestLabel "getHashMapDesctiprionTest"     getHashMapDesctiprionTest
    , TestLabel "iterateHashMapDBListTest"      iterateHashMapDBListTest
    , TestLabel "setDescriptionTest"            setDescriptionTest
    , TestLabel "getMaybeDataFieldTest"         getMaybeDataFieldTest
    , TestLabel "getMyValueTest"                getMyValueTest
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

addListTest =
    TestCase $
    assertEqual
    "for (addList [(\"access_key\",\"key\")] testEssenceList)"
    (EssenceList "person" "create" [("access_key","key"),("first_name","misha"),("last_name","dragon")])
    $ addList [("access_key","key")] testEssenceList

deletePairTest =
    TestCase $
    assertEqual
    "for (deletePair \"first_name\" testEssenceList)"
    (EssenceList "person" "create" [("last_name","dragon")])
    $ deletePair "first_name" testEssenceList

getEssenceDBTest =
    TestCase $
    assertEqual "for (getEssenceDB \"person\" \"create\" testConfig)"
    testEssenceDB
    $ getEssenceDB "person" "create" testConfig

getEssenceDB'Test =
    TestCase $
    assertEqual "for (getEssenceDB' \"person\" \"create\" testConfig)"
    testEssenceDatabase
    $ getEssenceDB' "person" "create" testConfig

getHashMapDesctiprionTest =
    TestCase $
    assertEqual "for (getHashMapDesctiprion (HM.fromList testEssenceDatabaseFields))"
    (HM.fromList testEssenceDBFields)
    $ getHashMapDesctiprion (HM.fromList testEssenceDatabaseFields)

iterateHashMapDBListTest =
    TestCase $
    assertEqual "for (iterateHashMapDBList testEssenceDatabaseFields)"
    testEssenceDBFields
    $ iterateHashMapDBList testEssenceDatabaseFields

setDescriptionTest =
    TestCase $
    assertEqual "for (setDescription testEssenceDatabaseDescription)"
    (Description (MyInteger 0) Nothing Nothing (Just PRIMARY))
    $ setDescription [("type","int"),("constraint","primary key")]

getMaybeDataFieldTest =
    TestCase $
    assertEqual "for (getMaybeDataField (Just \"null\"))"
    (Just NULL)
    $ getMaybeDataField (Just "null")

getMyValueTest =
    TestCase $
    assertEqual "for (getMyValue \"int\")"
    (MyInteger 0)
    $ getMyValue "int"


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

toEssenceListTest =
    TestCase $
    runReaderT (toEssenceList testEssenceDB
        [("first_name",Just "misha")
        ,("last_name",Just "dragon")
        ]) testConfig >>=
    assertEqual
    "for (toEssenceList testEssenceDB [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")])"
    (EssenceList "person" "create" [("first_name","'misha'"),("last_name","'dragon'")])