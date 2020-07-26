module Data.Request.Method.Methods.Test
    ( requestMethodMethodsTests
    ) where

import Config
import Data.Request.Method.Methods

import Test.HUnit

requestMethodMethodsTests =
    [ TestLabel "isFindActionTest"     isFindActionTest
    , TestLabel "isMethodCorrectTest"  isMethodCorrectTest
    ]

isFindActionTest =
    TestCase $
    assertEqual "for (isFindAction \"edit\" [\"edit\"])"
    True
    $ isFindAction "edit" ["edit"]

isMethodCorrectTest =
    TestCase $
    assertEqual "for (isMethodCorrect \"POST\" \"edit\" testConfig)"
    True
    $ isMethodCorrect "POST" "edit" testConfig
