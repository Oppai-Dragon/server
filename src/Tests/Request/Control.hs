module Tests.Request.Control
    ( requestControlTests
    ) where

import Config
import Data.Request.Access
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
    assertEqual "for (ifEveryoneUpdate testEssenceDB Everyone)"
    testEssenceDB
    $ ifEveryoneUpdate testEssenceDB Everyone

ifGetUpdateTest =
    TestCase $
    assertEqual "for (ifGetUpdate testEssenceDB)"
    testEssenceDB
    $ ifGetUpdate testEssenceDB

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