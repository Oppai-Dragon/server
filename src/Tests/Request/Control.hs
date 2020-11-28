module Tests.Request.Control
  ( requestControlTests
  ) where

import Config
import Data.Essence
import Data.Essence.Column
import Data.Request.Access
import Data.Request.Control

import qualified Data.HashMap.Strict as HM

import Tests.Essence
import Tests.Request

import Test.HUnit

requestControlTests :: [Test]
requestControlTests =
  [ TestLabel "isPathRequestCorrectTest" isPathRequestCorrectTest
  , TestLabel "ifNotEveryoneUpdateTest" ifNotEveryoneUpdateTest
  , TestLabel "ifGetUpdateTest" ifGetUpdateTest
  , TestLabel "parseRequestTest" parseRequestTest
  , TestLabel "isRequestCorrectTest" isRequestCorrectTest
  , TestLabel "getAccessArrTest" getAccessArrTest
  ]

isPathRequestCorrectTest, ifNotEveryoneUpdateTest, ifGetUpdateTest, parseRequestTest, isRequestCorrectTest, getAccessArrTest ::
     Test
isPathRequestCorrectTest =
  TestCase $
  assertEqual "for (isPathRequestCorrect testPersonCreateReq testApi)" True $
  isPathRequestCorrect testPersonCreateReq testApi

ifNotEveryoneUpdateTest =
  TestCase $
  assertEqual
    "for (ifNotEveryoneUpdate testAuthorGetColumn Author)"
    EssenceColumn
      { eColName = "author"
      , eColAction = "get"
      , eColHashMap =
          HM.fromList
            [ ( "id"
              , defaultColumn
                  {cValueType = BIGSERIAL, cConstraint = Just PrimaryKey})
            , ( "person_id"
              , defaultColumn
                  { cValueType = BIGINT
                  , cNULL = Just $ NOT NULL
                  , cRelations = Just $ Relations "person" "id"
                  , cConstraint = Just UNIQUE
                  })
            , ("description", defaultColumn {cValueType = VARCHAR 150})
            , ( "access_key"
              , defaultColumn {cValueType = UUID, cNULL = Just $ NOT NULL})
            ]
      } $
  ifNotEveryoneUpdate testAuthorGetColumn Author

ifGetUpdateTest =
  TestCase $
  assertEqual
    "for (ifGetUpdate testAuthorGetColumn)"
    EssenceColumn
      { eColName = "author"
      , eColAction = "get"
      , eColHashMap =
          HM.fromList
            [ ( "id"
              , defaultColumn
                  {cValueType = BIGSERIAL, cConstraint = Just PrimaryKey})
            , ("page", defaultColumn {cValueType = INT})
            , ( "person_id"
              , defaultColumn
                  { cValueType = BIGINT
                  , cNULL = Just $ NOT NULL
                  , cRelations = Just $ Relations "person" "id"
                  , cConstraint = Just UNIQUE
                  })
            , ("description", defaultColumn {cValueType = VARCHAR 150})
            ]
      } $
  ifGetUpdate testAuthorGetColumn

parseRequestTest =
  TestCase $
  parseRequest testPersonCreateReq >>=
  assertEqual
    "for (parseRequest testPersonCreateReq)"
    RequestInfo
      { reqIEssenceName = "person"
      , reqIAction = "create"
      , reqIQueryMBS =
          [("first_name", Just "misha"), ("last_name", Just "dragon")]
      , reqIMethod = "POST"
      }

isRequestCorrectTest =
  TestCase $
  isRequestCorrect testPersonCreateReq >>=
  assertEqual "for (isRequestCorrect testPersonCreateReq)" True .
  reqAnswerBool . reqAnswer

getAccessArrTest =
  TestCase $
  getAccessArr [("first_name", Just "misha"), ("last_name", Just "dragon")] >>=
  assertEqual
    "for (getAccessArr [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")])"
    [Everyone]
