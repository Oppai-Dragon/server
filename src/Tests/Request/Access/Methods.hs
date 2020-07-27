module Tests.Request.Access.Methods
    ( requestAccessMethodsTests
    ) where

import Config

import Data.Request.Access
import Data.Request.Access.Methods

import Data.Text (Text)

import Test.HUnit

requestAccessMethodsTests =
    [ TestLabel "isAccessTest" isAccessTest
    ]

isAccessTest =
    TestCase $
    assertEqual
    "for (isAccess \"category\" \"create\" [Admin] testApi)"
    True
    $ isAccess "category" "create" [Admin] testApi
