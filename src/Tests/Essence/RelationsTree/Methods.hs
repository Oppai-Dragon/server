module Tests.Essence.RelationsTree.Methods
  ( essenceRelationsTreeMethodsTests
  ) where

import Config

import Data.Essence.RelationsTree
import Data.Essence.RelationsTree.Methods
import Data.MyValue

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Test.HUnit

essenceRelationsTreeMethodsTests :: [Test]
essenceRelationsTreeMethodsTests =
  [ TestLabel "isEssenceRelationsTest" isEssenceRelationsTest
  , TestLabel "ifFieldsFillTest" ifFieldsFillTest
  , TestLabel "unpackLeafsTest" unpackLeafsTest
  , TestLabel "beforeUnderscoreTest" beforeUnderscoreTest
  , TestLabel "parseObjEssenceTest" parseObjEssenceTest
  , TestLabel "afterUnderscoreTest" afterUnderscoreTest
  , TestLabel "getListOfPairFromObjTest" getListOfPairFromObjTest
  , TestLabel "checkListTest" checkListTest
  , TestLabel "getNextFieldTest" getNextFieldTest
  , TestLabel "isRightRelationsTest" isRightRelationsTest
  , TestLabel "getIdPairFromObjTest" getIdPairFromObjTest
  ]

isEssenceRelationsTest, ifFieldsFillTest, unpackLeafsTest, beforeUnderscoreTest, parseObjEssenceTest, afterUnderscoreTest, getListOfPairFromObjTest, checkListTest, getNextFieldTest, isRightRelationsTest, getIdPairFromObjTest ::
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

unpackLeafsTest =
  TestCase $
  assertEqual
    "for (unpackLeafs \"person1\" [Leaf \"person_id\"] testObj)"
    [("person_id", MyInteger 1)] $
  unpackLeafs "person1" [Leaf "person_id"] testObj

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

checkListTest =
  TestCase $
  assertEqual
    "for (checkList \"person_id\" [(\"id\",\"1\")])"
    [("id", MyInteger 1)] $
  checkList "person_id" [("id", MyInteger 1)]

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

rTestObj, bTestObj, testObj :: A.Object
rTestObj = HM.fromList [("author1", A.object ["id" A..= A.Number 1])]

bTestObj =
  HM.fromList
    [("draft1", A.object ["author_id" A..= A.Number 2, "id" A..= A.Number 1])]

testObj = HM.fromList [("person1", A.object ["id" A..= A.Number 1])]
