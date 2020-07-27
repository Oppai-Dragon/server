module Tests.Request.Method.Methods
    ( requestMethodMethodsTests
    ) where

import Config
import Data.Request.Method.Methods

import Test.HUnit

requestMethodMethodsTests =
    [ TestLabel "isMethodCorrectTest"  isMethodCorrectTest
    ]

isMethodCorrectTest =
    TestCase $
    assertEqual "for (isMethodCorrect \"POST\" \"edit\" testApi)"
    True
    $ isMethodCorrect "POST" "edit" testApi
