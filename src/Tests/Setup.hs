module Tests.Setup
  ( setupTests
  ) where

import Setup

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Control.Monad.Trans.Writer.CPS

import Test.HUnit

setupTests :: [Test]
setupTests =
  [ TestLabel "getCreateQueryTest" getCreateQueryTest
  , TestLabel "parseAllQueriesTest" parseAllQueriesTest
  , TestLabel "iterateObjTest" iterateObjTest
  ]

getCreateQueryTest, parseAllQueriesTest, iterateObjTest :: Test
getCreateQueryTest =
  TestCase $
  assertEqual
    "for (getCreateQuery testSetupObj)"
    ("CREATE TABLE person " <>
     "( first_name VARCHAR(50) NOT NULL" <>
     " ,access_key UUID DEFAULT gen_random_uuid()" <>
     " ,date_of_creation DATE DEFAULT CURRENT_DATE" <>
     " ,last_name VARCHAR(50) NOT NULL" <>
     " ,is_admin BOOLEAN DEFAULT FALSE" <>
     " ,id BIGSERIAL PRIMARY KEY" <>
     " ,avatar VARCHAR(50) DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'" <>
     " );") $
  getCreateQuery testSetupObj

parseAllQueriesTest =
  TestCase $
  assertEqual
    "for (parseAllQueries \"TABLE person\")"
    "TABLE person ( id INT );" $
  parseAllQueries "TABLE person id INT ,"

iterateObjTest =
  TestCase $
  assertEqual
    "for (iterateObj testSetupObj)"
    ("TABLE person " <>
     "first_name VARCHAR(50) NOT NULL" <>
     " ,access_key UUID DEFAULT gen_random_uuid()" <>
     " ,date_of_creation DATE DEFAULT CURRENT_DATE" <>
     " ,last_name VARCHAR(50) NOT NULL" <>
     " ,is_admin BOOLEAN DEFAULT FALSE" <>
     " ,id BIGSERIAL PRIMARY KEY" <>
     " ,avatar VARCHAR(50) DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'" <>
     " ,") $
  execWriter (iterateObj testSetupObj)

testSetupObj, testPersonObj :: A.Object
testSetupObj =
  HM.fromList [("TABLE", A.object ["person" A..= A.Object testPersonObj])]

testPersonObj =
  HM.fromList
    [ ( "id"
      , (A.Array . V.fromList) [A.String "BIGSERIAL", A.String "PRIMARY KEY"])
    , ( "first_name"
      , (A.Array . V.fromList) [A.String "VARCHAR(50)", A.String "NOT NULL"])
    , ( "last_name"
      , (A.Array . V.fromList) [A.String "VARCHAR(50)", A.String "NOT NULL"])
    , ( "date_of_creation"
      , (A.Array . V.fromList)
          [A.String "DATE", A.String "DEFAULT CURRENT_DATE"])
    , ( "avatar"
      , (A.Array . V.fromList)
          [ A.String "VARCHAR(50)"
          , A.String "DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'"
          ])
    , ( "is_admin"
      , (A.Array . V.fromList) [A.String "BOOLEAN", A.String "DEFAULT FALSE"])
    , ( "access_key"
      , (A.Array . V.fromList)
          [A.String "UUID", A.String "DEFAULT gen_random_uuid()"])
    ]
