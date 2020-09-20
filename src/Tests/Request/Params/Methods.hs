module Tests.Request.Params.Methods
  ( requestParamsMethodsTests
  ) where

import Config
import Data.Essence.Column
import Data.MyValue
import Data.Request.Params.Methods
import Data.Required

import Data.Monoid

import Tests.Essence

import Test.HUnit

requestParamsMethodsTests :: [Test]
requestParamsMethodsTests =
  [ TestLabel "isRequiredParamsTest" isRequiredParamsTest
  , TestLabel "iterateRequiredParamsTest" iterateRequiredParamsTest
  , TestLabel "iterateParamsTest" iterateParamsTest
  , TestLabel "queryBSWithoutMaybeTest" queryBSWithoutMaybeTest
  , TestLabel "isTypeParamsCorrectTest" isTypeParamsCorrectTest
  , TestLabel "compareValueTypeTest" compareValueTypeTest
  ]

isRequiredParamsTest, iterateRequiredParamsTest, iterateParamsTest, queryBSWithoutMaybeTest, isTypeParamsCorrectTest, compareValueTypeTest ::
     Test
isRequiredParamsTest =
  TestCase $
  assertEqual
    "for (isRequiredParams testPersonCreateColumn [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")])"
    True $
  isRequiredParams
    testPersonCreateColumn
    [("first_name", Just "misha"), ("last_name", Just "dragon")]
    testApi

iterateRequiredParamsTest =
  TestCase $
  assertEqual
    "for (iterateRequiredParams (Required [AND [\"first_name\",\"last_name\"]]) [(\"first_name\",\"misha\"),(\"last_name\",\"dragon\")])"
    True $
  iterateRequiredParams
    (Required [AND ["first_name", "last_name"]])
    [("first_name", "misha"), ("last_name", "dragon")]

iterateParamsTest =
  TestCase $
  assertEqual
    "for (iterateParams [\"first_name\",\"last_name\"] [(\"first_name\",\"misha\"),(\"last_name\",\"dragon\")])"
    [True, True] $
  iterateParams
    ["first_name", "last_name"]
    [("first_name", "misha"), ("last_name", "dragon")]

queryBSWithoutMaybeTest =
  TestCase $
  assertEqual
    "for (queryBSWithoutMaybe [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")] [(\"first_name\",\"misha\"),(\"last_name\",\"dragon\")])"
    [("first_name", "misha"), ("last_name", "dragon")] $
  queryBSWithoutMaybe
    [("first_name", Just "misha"), ("last_name", Just "dragon")]

isTypeParamsCorrectTest =
  TestCase $
  assertEqual
    "for (isTypeParamsCorrect testPersonCreateColumn [(\"first_name\",MyString \"misha\"),(\"last_name\",MyString \"dragon\")])"
    (All True) $
  isTypeParamsCorrect
    testPersonCreateColumn
    [("first_name", MyString "misha"), ("last_name", MyString "dragon")]

compareValueTypeTest =
  TestCase $
  assertEqual "for (compareValueType INT (MyInteger 2))" True $
  compareValueType INT (MyInteger 2)
