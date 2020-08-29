module Tests.Essence.Methods
  ( essenceMethodsTests
  ) where

import Config
import Data.Essence
import Data.Essence.Methods
import Data.MyValue (MyValue(..))

import qualified Data.HashMap.Strict as HM

import Control.Monad.Trans.Reader

import Tests.Essence

import Test.HUnit

essenceMethodsTests :: [Test]
essenceMethodsTests =
  [ TestLabel "addListTest" addListTest
  , TestLabel "deletePairTest" deletePairTest
  , TestLabel "getEssenceFieldsTest" getEssenceFieldsTest
  , TestLabel "getEssenceDBTest" getEssenceDBTest
  , TestLabel "getEssenceDatabaseTest" getEssenceDatabaseTest
  , TestLabel "getHashMapDescriptionTest" getHashMapDescriptionTest
  , TestLabel "iterateHashMapDBListTest" iterateHashMapDBListTest
  , TestLabel "setDescriptionTest" setDescriptionTest
  , TestLabel "getMaybeDataFieldTest" getMaybeDataFieldTest
  , TestLabel "getMyValueTest" getMyValueTest
  , TestLabel "parseOnlyValuesTest" parseOnlyValuesTest
  , TestLabel "parseOnlyFieldsTest" parseOnlyFieldsTest
  , TestLabel "withoutEmptyTest" withoutEmptyTest
  , TestLabel "parseJustBSValueTest" parseJustBSValueTest
  , TestLabel "parseNothingBSValueTest" parseNothingBSValueTest
  , TestLabel "parseFieldValueTest" parseFieldValueTest
  , TestLabel "toEssenceListTest" toEssenceListTest
  ]

addListTest, deletePairTest, getEssenceFieldsTest, getEssenceDBTest, getEssenceDatabaseTest, getHashMapDescriptionTest, iterateHashMapDBListTest, setDescriptionTest, getMaybeDataFieldTest, getMyValueTest, parseOnlyValuesTest, parseOnlyFieldsTest, withoutEmptyTest, parseJustBSValueTest, parseNothingBSValueTest, parseFieldValueTest, toEssenceListTest ::
     Test
addListTest =
  TestCase $
  assertEqual
    "for (addList [(\"access_key\",\"key\")] testPersonListCreate)"
    (EssenceList
       "person"
       "create"
       [ ("first_name", MyString testFirstName)
       , ("last_name", MyString testLastName)
       , ("date_of_creation", MyDate testDate)
       , ("avatar", MyString testAvatar)
       , ("access_key", MyString testAccessKey)
       , ("is_admin", MyBool True)
       , ("access_key", MyString testAccessKey)
       ]) $
  addList [("access_key", MyString testAccessKey)] testPersonListCreate

deletePairTest =
  TestCase $
  assertEqual
    "for (deletePair \"first_name\" testPersonListCreate)"
    (EssenceList
       "person"
       "create"
       [ ("last_name", MyString testLastName)
       , ("date_of_creation", MyDate testDate)
       , ("avatar", MyString testAvatar)
       , ("access_key", MyString testAccessKey)
       , ("is_admin", MyBool True)
       ]) $
  deletePair "first_name" testPersonListCreate

getEssenceFieldsTest =
  TestCase $
  assertEqual
    "for (getEssenceFields testPersonCreateDB testApi)"
    ["avatar", "is_admin", "last_name", "first_name"] $
  getEssenceFields testPersonCreateDB testApi

getEssenceDBTest =
  TestCase $
  assertEqual
    "for (getEssenceDB \"person\" \"create\" testConfig testApi)"
    testPersonCreateDB $
  getEssenceDB "person" "create" testConfig testApi

getEssenceDatabaseTest =
  TestCase $
  assertEqual
    "for (getEssenceDatabase \"person\" testConfig testApi)"
    testPersonDatabase $
  getEssenceDatabase "person" testConfig testApi

getHashMapDescriptionTest =
  TestCase $
  assertEqual
    "for (getHashMapDescription (HM.fromList testPersonDatabaseFields))"
    (HM.fromList testPersonDBFields) $
  getHashMapDescription (HM.fromList testPersonDatabaseFields)

iterateHashMapDBListTest =
  TestCase $
  assertEqual
    "for (iterateHashMapDBList testPersonDatabaseFields)"
    testPersonDBFields $
  iterateHashMapDBList testPersonDatabaseFields

setDescriptionTest =
  TestCase $
  assertEqual
    "for (setDescription testPersonDatabaseDescription)"
    (Description (MyInteger 0) Nothing Nothing (Just PRIMARY)) $
  setDescription [("type", "int"), ("constraint", "primary key")]

getMaybeDataFieldTest =
  TestCase $
  assertEqual "for (getMaybeDataField (Just \"null\"))" (Just NULL) $
  getMaybeDataField (Just "null")

getMyValueTest =
  TestCase $
  assertEqual "for (getMyValue \"int\")" (MyInteger 0) $ getMyValue "int"

parseOnlyValuesTest =
  TestCase $
  assertEqual
    "for (parseOnlyValues [(\"field\",MyString \"value\")])"
    [MyString "value"] $
  parseOnlyValues [("field", MyString "value")]

parseOnlyFieldsTest =
  TestCase $
  assertEqual
    "for (parseOnlyFields [(\"field\",MyString \"values\")])"
    ["field"] $
  parseOnlyFields [("field", MyString "values")]

withoutEmptyTest =
  TestCase $
  assertEqual "for (withoutEmpty [(\"field\", MyEmpty)])" [] $
  withoutEmpty [("field", MyEmpty)]

parseJustBSValueTest =
  TestCase $
  assertEqual
    "for (parseBSValue \"id\" [(\"id\", Just \"1\")])"
    ("id", MyInteger 1) $
  parseBSValue "id" [("id", Just "1")]

parseNothingBSValueTest =
  TestCase $
  assertEqual "for (parseBSValue \"id\" [(\"id\", Nothing)])" ("id", MyEmpty) $
  parseBSValue "id" [("id", Nothing)]

parseFieldValueTest =
  TestCase $
  assertEqual
    "for (parseFieldValue [(\"id\", Just \"1\"), (\"first_name\", Nothing)])"
    [("id", MyInteger 1), ("first_name", MyEmpty)] $
  parseFieldValue
    ["id", "first_name"]
    [("id", Just "1"), ("first_name", Nothing)]

toEssenceListTest =
  TestCase $
  runReaderT
    (toEssenceList
       testPersonCreateDB
       [("first_name", Just "misha"), ("last_name", Just "dragon")])
    testHandle >>=
  assertEqual
    "for (toEssenceList testPersonCreateDB [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")])"
    (EssenceList
       "person"
       "create"
       [("last_name", MyString "dragon"), ("first_name", MyString "misha")])
