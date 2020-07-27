module Data.Request.Control.Test
    ( requestControlTests
    ) where

import Config
import Data.Access
import Data.Essence
import Data.MyValue
import Data.Request.Control

import qualified Data.HashMap.Strict as HM

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
    assertEqual "for (isPathRequestCorrect testPostReq testApi)"
    True
    $ isPathRequestCorrect testPostReq testApi

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
    assertEqual "for (parseRequest testPostReq)"
    ("person","create",[("first_name",Just "misha"),("last_name",Just "dragon")],"POST")
    $ parseRequest testPostReq

isRequestCorrectTest =
    TestCase $
    runReaderT isRequestCorrect testConfig >>=
    assertEqual "for (isRequestCorrect testPostReq)" (True,notFound)

getAccessArrTest =
    TestCase $
    getAccessArrTest [("first_name",Just "misha"),("last_name",Just "dragon")] >>=
    assertEqual "for (getAccessArr [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")])"
    [Everyone]