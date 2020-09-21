module Tests.Setup
  ( setupTests
  ) where

import Setup

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Tests.Essence

import Test.HUnit

setupTests :: [Test]
setupTests = [TestLabel "getCreateQueryTest" getCreateQueryTest]

getCreateQueryTest :: Test
getCreateQueryTest =
  TestCase $
  assertEqual
    "for (getCreateQuery testPersonObj)"
    ("CREATE TABLE person " <>
     "( first_name VARCHAR(50) NOT NULL," <>
     " access_key UUID DEFAULT gen_random_uuid()," <>
     " date_of_creation DATE DEFAULT CURRENT_DATE," <>
     " last_name VARCHAR(50) NOT NULL," <>
     " is_admin BOOLEAN DEFAULT FALSE," <>
     " id BIGSERIAL PRIMARY KEY," <>
     " avatar VARCHAR(50) DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'" <>
     " );") $
  getCreateQuery testPersonObj

testPersonObj :: A.Object
testPersonObj = HM.singleton "person" $ A.Object testPersonColumnObj
