module Tests.Request.Control
    ( requestControlTests
    ) where

import Config
import Data.Request.Access
import Data.Empty
import Data.Essence
import Data.MyValue
import Data.Request.Control

import qualified Data.HashMap.Strict as HM
import           Control.Monad.Trans.Reader

import Tests.Essence
import Tests.Request

import Test.HUnit

requestControlTests =
    [ TestLabel "isPathRequestCorrectTest"  isPathRequestCorrectTest
    , TestLabel "ifEveryoneUpdateTest"      ifEveryoneUpdateTest
    , TestLabel "ifGetUpdateTest"           ifGetUpdateTest
    , TestLabel "parseRequestTest"          parseRequestTest
    , TestLabel "isRequestCorrectTest"      isRequestCorrectTest
    , TestLabel "getAccessArrTest"          getAccessArrTest
    ]

isPathRequestCorrectTest =
    TestCase $
    assertEqual "for (isPathRequestCorrect testPersonCreateReq testApi)"
    True
    $ isPathRequestCorrect testPersonCreateReq testApi

ifEveryoneUpdateTest =
    TestCase $
    assertEqual "for (ifEveryoneUpdate testAuthorGetDB Author)"
    (EssenceDB "author" "get" $ HM.fromList
    [("id",Description (MyInteger 0) Nothing Nothing (Just PRIMARY))
    ,("person_id",Description (MyInteger 0) (Just $ NOT NULL) (Just $ Relations "person" "id") (Just UNIQUE))
    ,("description",Description (MyString "") Nothing Nothing Nothing)
    ,("access_key",Description (MyString empty) (Just $ NOT NULL) Nothing Nothing)
    ]) $ ifEveryoneUpdate testAuthorGetDB Author

ifGetUpdateTest =
    TestCase $
    assertEqual "for (ifGetUpdate testAuthorGetDB)"
    (EssenceDB "author" "get" $ HM.fromList
    [("id",Description (MyInteger 0) Nothing Nothing (Just PRIMARY))
    ,("page",Description (MyInteger empty) Nothing Nothing Nothing)
    ,("person_id",Description (MyInteger 0) (Just $ NOT NULL) (Just $ Relations "person" "id") (Just UNIQUE))
    ,("description",Description (MyString "") Nothing Nothing Nothing)
    ]) $ ifGetUpdate testAuthorGetDB

parseRequestTest =
    TestCase $
    assertEqual "for (parseRequest testPersonCreateReq)"
    ("person","create",[("first_name",Just "misha"),("last_name",Just "dragon")],"POST")
    $ parseRequest testPersonCreateReq

isRequestCorrectTest =
    TestCase $
    runReaderT (isRequestCorrect testPersonCreateReq) testConfig >>=
    assertEqual "for (isRequestCorrect testPersonCreateReq)" True . fst

getAccessArrTest =
    TestCase $
    getAccessArr [("first_name",Just "misha"),("last_name",Just "dragon")] >>=
    assertEqual
    "for (getAccessArr [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")])"
    [Everyone]