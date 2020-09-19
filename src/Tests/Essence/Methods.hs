module Tests.Essence.Methods
  ( essenceMethodsTests
  ) where

import Config
import Data.Essence
import Data.Essence.Column
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
  , TestLabel "getEssenceColumnTest" getEssenceColumnTest
  , TestLabel "getHashMapColumnTest" getHashMapColumnTest
  , TestLabel "iterateHashMapJsonListTest" iterateHashMapJsonListTest
  , TestLabel "setColumnTest" setColumnTest
  , TestLabel "getMaybeDataFieldTest" getMaybeDataFieldTest
  , TestLabel "parseOnlyValuesTest" parseOnlyValuesTest
  , TestLabel "parseOnlyFieldsTest" parseOnlyFieldsTest
  , TestLabel "withoutEmptyTest" withoutEmptyTest
  , TestLabel "parseJustBSValueTest" parseJustBSValueTest
  , TestLabel "parseNothingBSValueTest" parseNothingBSValueTest
  , TestLabel "parseFieldValueTest" parseFieldValueTest
  , TestLabel "toEssenceListTest" toEssenceListTest
  ]

addListTest, deletePairTest, getEssenceFieldsTest, getEssenceColumnTest, getEssenceDatabaseTest, getHashMapColumnTest, iterateHashMapJsonListTest, setColumnTest, getMaybeDataFieldTest, parseOnlyValuesTest, parseOnlyFieldsTest, withoutEmptyTest, parseJustBSValueTest, parseNothingBSValueTest, parseFieldValueTest, toEssenceListTest ::
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
    ["avatar", "last_name", "first_name"] $
  getEssenceFields testPersonCreateDB testApi

getEssenceColumnTest =
  TestCase $
  assertEqual
    "for (getEssenceColumn \"person\" \"create\" testConfig testApi)"
    testPersonCreateDB $
  getEssenceColumn "person" "create" testConfig testApi

getHashMapColumnTest =
  TestCase $
  assertEqual
    "for (getHashMapColumn (HM.fromList testPersonColumnFields))"
    (HM.fromList testPersonColumnFields) $
  getHashMapColumn (HM.fromList testPersonColumnFields)

iterateHashMapJsonListTest =
  TestCase $
  assertEqual
    "for (iterateHashMapDBList testPersonColumnFields)"
    testPersonColumnFields $
  iterateHashMapDBList testPersonColumnFields

setColumnTest =
  TestCase $
  assertEqual
    "for (setColumn testPersonDatabaseColumn)"
    defaultColumn {cValueType = DATE, cDefault = JUST $ Default "CURRENT_DATE"} .
  setColumn $
  A.object ["type" A..= A.String "DATE", "default" A..= A.String "CURRENT_DATE"]

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
