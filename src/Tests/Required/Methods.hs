module Tests.Required.Methods
  ( requiredMethodsTests
  ) where

import Config
import Data.Required
import Data.Required.Methods

import Tests.Essence

import Test.HUnit

requiredMethodsTests :: [Test]
requiredMethodsTests =
  [ TestLabel "toFieldsTest" toFieldsTest
  , TestLabel "getRequiredFieldsTest" getRequiredFieldsTest
  , TestLabel "requiredSequenceATest" requiredSequenceATest
  , TestLabel "requiredApplyTest" requiredApplyTest
  ]

toFieldsTest, getRequiredFieldsTest, requiredSequenceATest, requiredApplyTest ::
     Test
toFieldsTest =
  TestCase $
  assertEqual
    "for (toFields (Required [AND [\"first_name\",\"last_name\"],OR [\"avatar\"]]))"
    (["first_name", "last_name", "avatar"] :: [String]) $
  toFields (Required [AND ["first_name", "last_name"], OR ["avatar"]])

getRequiredFieldsTest =
  TestCase $
  assertEqual
    "for (getRequiredFields testPersonCreateColumn)"
    (Required [AND ["last_name", "first_name"]] :: Required [String]) $
  getRequiredFields testPersonCreateColumn testApi

requiredSequenceATest =
  TestCase $
  assertEqual
    "for (requiredSequenceA (Required [AND \"id\",AND \"access_key\"]))"
    (AND ["id", "access_key"] :: Required [String]) $
  requiredSequenceA [AND ["id"], AND ["access_key"]]

requiredApplyTest =
  TestCase $
  assertEqual
    "for (requiredApply (AND (\"id\":)) (AND [\"access_key\"]))"
    (AND ["id", "access_key"] :: Required [String]) $
  requiredApply (AND (["id"] <>)) (AND ["access_key"])
