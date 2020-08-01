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
    , TestLabel "getEssenceFieldsTest"          getEssenceFieldsTest
    , TestLabel "getEssenceDBTest"              getEssenceDBTest
    , TestLabel "getEssenceDatabaseTest"        getEssenceDatabaseTest
    , TestLabel "getHashMapDescriptionTest"     getHashMapDescriptionTest
    , TestLabel "iterateHashMapDBListTest"      iterateHashMapDBListTest
    , TestLabel "setDescriptionTest"            setDescriptionTest
    , TestLabel "getMaybeDataFieldTest"         getMaybeDataFieldTest
    , TestLabel "getMyValueTest"                getMyValueTest
    , TestLabel "parseOnlyValuesTest"           parseOnlyValuesTest
    , TestLabel "parseOnlyFieldsTest"           parseOnlyFieldsTest
    , TestLabel "withoutEmptyTest"              withoutEmptyTest
    , TestLabel "parse_Just_BSValueTest"        parse_Just_BSValueTest
    , TestLabel "parse_Nothing_BSValueTest"     parse_Nothing_BSValueTest
    , TestLabel "parseFieldValueTest"           parseFieldValueTest
    , TestLabel "toEssenceListTest"             toEssenceListTest
    ]

addListTest =
    TestCase $
    assertEqual
    "for (addList [(\"access_key\",\"key\")] testEssenceList)"
    (EssenceList "person" "create" [("access_key",MyString "key"),("first_name",MyString "misha"),("last_name",MyString "dragon")])
    $ addList [("access_key",MyString "key")] testEssenceList

deletePairTest =
    TestCase $
    assertEqual
    "for (deletePair \"first_name\" testEssenceList)"
    (EssenceList "person" "create" [("last_name",MyString "dragon")])
    $ deletePair "first_name" testEssenceList

getEssenceFieldsTest =
    TestCase $
    assertEqual "for (getEssenceFields testEssenceDB testApi)"
    ["avatar","is_admin","last_name","first_name"]
    $ getEssenceFields testEssenceDB testApi

getEssenceDBTest =
    TestCase $
    assertEqual "for (getEssenceDB \"person\" \"create\" testConfig testApi)"
    testEssenceDB
    $ getEssenceDB "person" "create" testConfig testApi

getEssenceDatabaseTest =
    TestCase $
    assertEqual "for (getEssenceDatabase \"person\" \"create\" testConfig testApi)"
    testEssenceDatabase
    $ getEssenceDatabase "person" "create" testConfig testApi

getHashMapDescriptionTest =
    TestCase $
    assertEqual "for (getHashMapDescription (HM.fromList testEssenceDatabaseFields))"
    (HM.fromList testEssenceDBFields)
    $ getHashMapDescription (HM.fromList testEssenceDatabaseFields)

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

parseOnlyValuesTest =
    TestCase $
    assertEqual "for (parseOnlyValues [(\"field\",MyString \"value\")])"
    [MyString "value"]
    $ parseOnlyValues [("field",MyString "value")]

parseOnlyFieldsTest =
    TestCase $
    assertEqual "for (parseOnlyFields [(\"field\",MyString \"values\")])"
    ["field"]
    $ parseOnlyFields [("field",MyString "values")]

withoutEmptyTest =
    TestCase $
    assertEqual "for (withoutEmpty [(\"field\", MyEmpty)])"
    []
    $ withoutEmpty [("field", MyEmpty)]

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
    (EssenceList "person" "create" [("first_name",MyString "misha"),("last_name",MyString "dragon")])