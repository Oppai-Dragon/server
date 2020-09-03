module Tests.Essence.RelationsTree.Methods
  ( essenceRelationsTreeMethodsTests
  ) where

import Config

import Data.Essence.RelationsTree
import Data.Essence.RelationsTree.Methods
import Data.MyValue

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Test.HUnit

essenceRelationsTreeMethodsTests :: [Test]
essenceRelationsTreeMethodsTests =
  [ TestLabel "isEssenceRelationsTest" isEssenceRelationsTest
  , TestLabel "ifFieldsFillTest" ifFieldsFillTest
  , TestLabel "getAddedFieldsTest" getAddedFieldsTest
  , TestLabel "unpackLeafsTest" unpackLeafsTest
  , TestLabel "beforeUnderscoreTest" beforeUnderscoreTest
  , TestLabel "parseObjEssenceTest" parseObjEssenceTest
  , TestLabel "afterUnderscoreTest" afterUnderscoreTest
  , TestLabel "getListOfPairFromObjTest" getListOfPairFromObjTest
  , TestLabel "getNextFieldTest" getNextFieldTest
  , TestLabel "isRightRelationsTest" isRightRelationsTest
  , TestLabel "getIdPairFromObjTest" getIdPairFromObjTest
  ]

isEssenceRelationsTest, ifFieldsFillTest, getAddedFieldsTest, unpackLeafsTest, beforeUnderscoreTest, parseObjEssenceTest, afterUnderscoreTest, getListOfPairFromObjTest, getNextFieldTest, isRightRelationsTest, getIdPairFromObjTest ::
     Test
isEssenceRelationsTest =
  TestCase $
  assertEqual "for (isEssenceRelations \"draft\" testApi)" True $
  isEssenceRelations "draft" testApi

ifFieldsFillTest =
  TestCase $
  assertEqual
    "for (ifFieldsFill [\"name\",\"category_id\"] [(\"name\",MyString \"\"),(\"category\",MyInteger 0)])"
    True $
  ifFieldsFill
    ["name", "category_id"]
    [("name", MyString ""), ("category", MyInteger 0)]

getAddedFieldsTest =
  TestCase $
  assertEqual
    "for (getAddedFields \"draft\"[Leaf \"draft_id\",Leaf \"author_id\",Leaf \"category_id\",Leaf \"main_photo\",Leaf \"content\",Leaf \"name\",Leaf \"tag_ids\",Leaf \"optional_photos\"] (A.Object testDraftFiedlsObj))"
    [ ("draft_id", MyInteger 24)
    , ("author_id", MyInteger 24)
    , ("category_id", MyInteger 24)
    , ("main_photo", MyEmpty)
    , ("content", MyString "testContent")
    , ("name", MyString "testDraft")
    , ("tag_ids", MyIntegers [24])
    , ("optional_photos", MyEmpty)
    ] $
  getAddedFields
    "draft"
    [ Leaf "draft_id"
    , Leaf "author_id"
    , Leaf "category_id"
    , Leaf "main_photo"
    , Leaf "content"
    , Leaf "name"
    , Leaf "tag_ids"
    , Leaf "optional_photos"
    ]
    testDraftObj

unpackLeafsTest =
  TestCase $
  assertEqual
    "for (unpackLeafs [Leaf \"draft_id\",Leaf \"author_id\",Leaf \"category_id\",Leaf \"main_photo\",Leaf \"content\",Leaf \"name\",Leaf \"tag_ids\",Leaf \"optional_photos\"] HM.insert \"draft_id\" (A.Number 24) testDraftFiedlsObj)"
    [ ("draft_id", MyInteger 24)
    , ("author_id", MyInteger 24)
    , ("category_id", MyInteger 24)
    , ("main_photo", MyEmpty)
    , ("content", MyString "testContent")
    , ("name", MyString "testDraft")
    , ("tag_ids", MyIntegers [24])
    , ("optional_photos", MyEmpty)
    ] .
  unpackLeafs
    [ Leaf "draft_id"
    , Leaf "author_id"
    , Leaf "category_id"
    , Leaf "main_photo"
    , Leaf "content"
    , Leaf "name"
    , Leaf "tag_ids"
    , Leaf "optional_photos"
    ] $
  HM.insert "draft_id" (A.Number 24) testDraftFiedlsObj

beforeUnderscoreTest =
  TestCase $
  assertEqual "for (beforeUnderscore \"misha_lox\")" "misha" $
  beforeUnderscore "misha_lox"

parseObjEssenceTest =
  TestCase $
  assertEqual "for (parseObjEssence \"person\")" "person1" $
  parseObjEssence "person"

afterUnderscoreTest =
  TestCase $
  assertEqual "for (afterUnderscore \"misha_lox\")" "lox" $
  afterUnderscore "misha_lox"

getListOfPairFromObjTest =
  TestCase $
  assertEqual
    "for (getListOfPairFromObj \"person_id\" testObj)"
    [("person_id", MyInteger 1)] $
  getListOfPairFromObj "person_id" testObj

getNextFieldTest =
  TestCase $
  assertEqual "for (getNextField (Trunk \"kuk\" (Branch \"kek\" [])))" "kek" $
  getNextField (Trunk "kuk" (Branch "kek" []))

isRightRelationsTest =
  TestCase $
  assertEqual
    "for (isRightRelations rTestObj bTestObj \"author_id\" \"draft\")"
    False $
  isRightRelations rTestObj bTestObj "author_id" "draft"

getIdPairFromObjTest =
  TestCase $
  assertEqual "for (getIdPairFromObj \"draft\" bTestObj)" [("id", MyInteger 1)] $
  getIdPairFromObj "draft" bTestObj

rTestObj, bTestObj, testDraftObj, testDraftFiedlsObj, testObj :: A.Object
rTestObj = HM.fromList [("author1", A.object ["id" A..= A.Number 1])]

bTestObj =
  HM.fromList
    [("draft1", A.object ["author_id" A..= A.Number 2, "id" A..= A.Number 1])]

testDraftObj = HM.singleton "draft1" $ A.Object testDraftFiedlsObj

testDraftFiedlsObj =
  HM.fromList
    [ ("author_id", A.Number 24.0)
    , ("category_id", A.Number 24.0)
    , ("main_photo", A.Null)
    , ("content", A.String "testContent")
    , ("date_of_creation", A.String "2020-09-02")
    , ("name", A.String "testDraft")
    , ("id", A.Number 24.0)
    , ("tag_ids", A.Array $ V.fromList [A.Number 24.0])
    , ("optional_photos", A.Null)
    ]

testObj = HM.fromList [("person1", A.object ["id" A..= A.Number 1])]
