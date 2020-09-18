module Tests.Request.Control
  ( requestControlTests
  ) where

import Config
import Data.Base
import Data.Empty
import Data.Essence
import Data.MyValue
import Data.Request.Access
import Data.Request.Control

import qualified Data.HashMap.Strict as HM

import Tests.Essence
import Tests.Request

import Test.HUnit

requestControlTests :: [Test]
requestControlTests =
  [ TestLabel "isPathRequestCorrectTest" isPathRequestCorrectTest
  , TestLabel "ifEveryoneUpdateTest" ifEveryoneUpdateTest
  , TestLabel "ifGetUpdateTest" ifGetUpdateTest
  , TestLabel "parseRequestTest" parseRequestTest
  , TestLabel "isRequestCorrectTest" isRequestCorrectTest
  , TestLabel "getAccessArrTest" getAccessArrTest
  ]

isPathRequestCorrectTest, ifEveryoneUpdateTest, ifGetUpdateTest, parseRequestTest, isRequestCorrectTest, getAccessArrTest ::
     Test
isPathRequestCorrectTest =
  TestCase $
  assertEqual "for (isPathRequestCorrect testPersonCreateReq testApi)" True $
  isPathRequestCorrect testPersonCreateReq testApi

ifEveryoneUpdateTest =
  TestCase $
  assertEqual
    "for (ifEveryoneUpdate testAuthorGetDB Author)"
    (EssenceColumn "author" "get" $
     HM.fromList
       [ ("id", Column (MyInteger 0) Nothing Nothing (Just PRIMARY))
       , ( "person_id"
         , Column
             (MyInteger 0)
             (Just $ NOT NULL)
             (Just $ Relations "person" "id")
             (Just UNIQUE))
       , ("Column", Column (MyString "") Nothing Nothing Nothing)
       , ( "access_key"
         , Column (MyString empty) (Just $ NOT NULL) Nothing Nothing)
       ]) $
  ifEveryoneUpdate testAuthorGetDB Author

ifGetUpdateTest =
  TestCase $
  assertEqual
    "for (ifGetUpdate testAuthorGetDB)"
    (EssenceColumn "author" "get" $
     HM.fromList
       [ ("id", Column (MyInteger 0) Nothing Nothing (Just PRIMARY))
       , ("page", Column (MyInteger empty) Nothing Nothing Nothing)
       , ( "person_id"
         , Column
             (MyInteger 0)
             (Just $ NOT NULL)
             (Just $ Relations "person" "id")
             (Just UNIQUE))
       , ("Column", Column (MyString "") Nothing Nothing Nothing)
       ]) $
  ifGetUpdate testAuthorGetDB

parseRequestTest =
  TestCase $
  parseRequest testPersonCreateReq >>=
  assertEqual
    "for (parseRequest testPersonCreateReq)"
    ( "person"
    , "create"
    , [("first_name", Just "misha"), ("last_name", Just "dragon")]
    , "POST")

isRequestCorrectTest =
  TestCase $
  isRequestCorrect testPersonCreateReq >>=
  assertEqual "for (isRequestCorrect testPersonCreateReq)" True . fst4

getAccessArrTest =
  TestCase $
  getAccessArr [("first_name", Just "misha"), ("last_name", Just "dragon")] >>=
  assertEqual
    "for (getAccessArr [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")])"
    [Everyone]
