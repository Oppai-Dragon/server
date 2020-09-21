module Tests.Database.Test
  ( databaseTestTests
  ) where

import Config

import Data.Essence
import Data.MyValue
import Database.Test

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Tests.Essence

import Test.HUnit

databaseTestTests :: [Test]
databaseTestTests =
  [ TestLabel "getDefaultValuesTest" getDefaultValuesTest
  , TestLabel "buildCreateEssenceValueTest" buildCreateEssenceValueTest
  , TestLabel "buildEditEssenceValueTest" buildEditEssenceValueTest
  , TestLabel "buildGetEssenceValueTest" buildGetEssenceValueTest
  , TestLabel "buildDeleteEssenceValueTest" buildDeleteEssenceValueTest
  , TestLabel "updateHmListTest" updateHmListTest
  , TestLabel "updateEssenceListTest" updateEssenceListTest
  , TestLabel "getNeededFieldsDraftTest" getNeededFieldsDraftTest
  , TestLabel "getNeededFieldsPersonTest" getNeededFieldsPersonTest
  , TestLabel "updateDataTest" updateDataTest
  , TestLabel "getNewsRelatedFieldsTest" getNewsRelatedFieldsTest
  , TestLabel "getOtherRelatedFieldsTest" getOtherRelatedFieldsTest
  , TestLabel "handleDraftCaseTest" handleDraftCaseTest
  ] <>
  chooseNameForAddingTests

getDefaultValuesTest, buildCreateEssenceValueTest, buildEditEssenceValueTest, buildGetEssenceValueTest, buildDeleteEssenceValueTest, updateHmListTest, updateEssenceListTest, getNeededFieldsDraftTest, getNeededFieldsPersonTest, updateDataTest, getNewsRelatedFieldsTest, getOtherRelatedFieldsTest, handleDraftCaseTest ::
     Test
getDefaultValuesTest =
  TestCase $
  assertEqual
    "for (getDefaultValues [\"first_name\",\"last_name\",\"date_of_creation\",\"avatar\",\"is_admin\",\"access_key\"])"
    [ ("first_name", A.String defaultFirstName)
    , ("last_name", A.String defaultLastName)
    , ("date_of_creation", A.String defaultDate)
    , ("avatar", A.String defaultAvatar)
    , ("is_admin", A.Bool True)
    , ("access_key", A.String defaultAccessKey)
    ] $
  getDefaultValues
    [ "first_name"
    , "last_name"
    , "date_of_creation"
    , "avatar"
    , "is_admin"
    , "access_key"
    ]

buildCreateEssenceValueTest =
  TestCase $
  assertEqual
    "for (buildEssenceValue (EssenceList \"author\" \"create\" []))"
    (A.object ["author1" A..= A.Null]) $
  buildEssenceValue (EssenceList "author" "create" [])

buildEditEssenceValueTest =
  TestCase $
  assertEqual
    "for (buildEssenceValue (EssenceList \"author\" \"edit\" []))"
    goodResultValue $
  buildEssenceValue (EssenceList "author" "edit" [])

buildGetEssenceValueTest =
  TestCase $
  assertEqual
    "for (buildEssenceValue (EssenceList \"author\" \"get\" []))"
    (A.object ["author1" A..= A.Null]) $
  buildEssenceValue (EssenceList "author" "get" [])

buildDeleteEssenceValueTest =
  TestCase $
  assertEqual
    "for (buildEssenceValue (EssenceList \"author\" \"delete\" []))"
    goodResultValue $
  buildEssenceValue (EssenceList "author" "edit" [])

updateHmListTest =
  TestCase $
  assertEqual
    "for (updateHmList \"author\" [(\"person_id\",Number 1.0),(\"author_id\",Number 1.0),(\"description\",Null)])"
    [("person_id", A.Number 1), ("id", A.Number 1), ("description", A.Null)] $
  updateHmList
    "author"
    [ ("person_id", A.Number 1)
    , ("author_id", A.Number 1)
    , ("description", A.Null)
    ]

updateEssenceListTest =
  TestCase $
  assertEqual
    "for (updateEssenceList \"author\" [(\"person_id\",MyInteger 1),(\"author_id\",MyInteger 1),(\"description\",MyEmpty)])"
    [("person_id", MyInteger 1), ("id", MyInteger 1), ("description", MyEmpty)] $
  updateEssenceList
    "author"
    [ ("person_id", MyInteger 1)
    , ("author_id", MyInteger 1)
    , ("description", MyEmpty)
    ]

getNeededFieldsDraftTest =
  TestCase $
  assertEqual
    "for (getNeededFields \"draft\" testDraftObj)"
    [ ("author_id", MyInteger 2)
    , ("category_id", MyInteger 3)
    , ("content", MyString "for loxov")
    , ("date_of_creation", MyDate "2020-09-03")
    , ("name", MyString "testDraft")
    , ("id", MyInteger 1)
    , ("tag_ids", MyIntegerArr [4])
    ] $
  getNeededFields "draft" testDraftObj

getNeededFieldsPersonTest =
  TestCase $
  assertEqual
    "for (getNeededFields \"person\" testPersonObj)"
    [("id", MyInteger 1)] $
  getNeededFields "person" testPersonObj

updateDataTest =
  TestCase $
  runReaderT
    (execStateT (updateData "person" (A.Object testPersonObj)) [])
    testHandle >>=
  assertEqual
    "runReaderT (execStateT (updateData \"person\" testPersonObj) []) (Config HM.empty)"
    [("person", [("id", MyInteger 1)])]

chooseNameForAddingTests :: [Test]
chooseNameForAddingTests =
  [ TestLabel "authorChooseNameForAddingTest" authorChooseNameForAddingTest
  , TestLabel "draftChooseNameForAddingTest" draftChooseNameForAddingTest
  , TestLabel "newsChooseNameForAddingTest" newsChooseNameForAddingTest
  , TestLabel "commentChooseNameForAddingTest" commentChooseNameForAddingTest
  , TestLabel "otherChooseNameForAddingTest" otherChooseNameForAddingTest
  ]

authorChooseNameForAddingTest, draftChooseNameForAddingTest, newsChooseNameForAddingTest, commentChooseNameForAddingTest, otherChooseNameForAddingTest ::
     Test
authorChooseNameForAddingTest =
  TestCase $
  assertEqual "for (chooseNameForAdding \"author\")" ["person"] $
  chooseNameForAdding "author"

draftChooseNameForAddingTest =
  TestCase $
  assertEqual
    "for (chooseNameForAdding \"draft\")"
    ["author", "category", "tag"] $
  chooseNameForAdding "draft"

newsChooseNameForAddingTest =
  TestCase $
  assertEqual "for (chooseNameForAdding \"news\")" ["draft"] $
  chooseNameForAdding "news"

commentChooseNameForAddingTest =
  TestCase $
  assertEqual "for (chooseNameForAdding \"comment\")" ["person", "news"] $
  chooseNameForAdding "comment"

otherChooseNameForAddingTest =
  TestCase $
  assertEqual "for (chooseNameForAdding \"person\")" ["person"] $
  chooseNameForAdding "person"

getNewsRelatedFieldsTest =
  TestCase $
  assertEqual
    "for (getRelatedFields \"news\" testEssenceData)"
    [ ("draft_id", MyInteger 1)
    , ("name", MyString "testDraft")
    , ("date_of_creation", MyDate "2020-09-03")
    , ("author_id", MyInteger 2)
    , ("category_id", MyInteger 3)
    , ("tag_ids", MyIntegerArr [4])
    , ("content", MyString "for loxov")
    , ("main_photo", MyEmpty)
    , ("optional_photos", MyEmpty)
    ] $
  getRelatedFields "news" testEssenceData

getOtherRelatedFieldsTest =
  TestCase $
  assertEqual
    "for (getRelatedFields \"person\" testEssenceData)"
    [("person_id", MyInteger 1)] $
  getRelatedFields "person" testEssenceData

handleDraftCaseTest =
  TestCase $
  assertEqual
    "for (handleDraftCase \"draft\" [(\"tag_id\",MyInteger 1)])"
    [("tag_ids", MyIntegerArr [1])] $
  handleDraftCase "draft" [("tag_id", MyInteger 1)]

testEssenceData :: [(String, List)]
testEssenceData = [("person", testPersonList), ("draft", testDraftList)]

testPersonObj :: A.Object
testPersonObj =
  HM.singleton "person1" $
  A.object
    [ "id" A..= A.Number 1
    , "first_name" A..= A.String "misha"
    , "last_name" A..= A.String "dragon"
    , "date_of_creation" A..= A.String "2020-08-03"
    , "avatar" A..= A.String "uri"
    , "is_admin" A..= A.Bool True
    , "access_key" A..= A.String "key"
    ]

testPersonList :: List
testPersonList = [("id", MyInteger 1)]

testDraftObj :: A.Object
testDraftObj =
  HM.singleton "draft1" $
  A.object
    [ "id" A..= A.Number 1
    , "name" A..= A.String "testDraft"
    , "date_of_creation" A..= A.String "2020-09-03"
    , "author_id" A..= A.Number 2
    , "category_id" A..= A.Number 3
    , "tag_ids" A..= (A.Array . V.fromList) [A.Number 4]
    , "content" A..= A.String "for loxov"
    , "main_photo" A..= A.Null
    , "optional_photos" A..= A.Null
    ]

testDraftList :: List
testDraftList =
  [ ("id", MyInteger 1)
  , ("name", MyString "testDraft")
  , ("date_of_creation", MyDate "2020-09-03")
  , ("author_id", MyInteger 2)
  , ("category_id", MyInteger 3)
  , ("tag_ids", MyIntegerArr [4])
  , ("content", MyString "for loxov")
  , ("main_photo", MyEmpty)
  , ("optional_photos", MyEmpty)
  ]
