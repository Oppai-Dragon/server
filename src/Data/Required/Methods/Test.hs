module Data.Required.Methods.Test
    ( requiredMethodsTests
    ) where

import Data.Required
import Data.Required.Methods

import Data.Aeson
import Data.Vector (fromList)
import Data.HashMap.Strict (singleton)

import Test.HUnit

requiredMethodsTests =
    [ TestLabel "getAllFieldsTest"  getAllFieldsTest
    ]

getAllFieldsTest =
    TestCase $
    assertEqual "for (getAllFields )"
    ["test","test","test"]
    $ getAllFields testRequiredFields

testRequiredFields =
    Required
    [ AND ["test"]
    , OR ["test"]
    , Optional
        ( singleton
        "filter"
        $ Array (fromList ["test"])
        )
    ]
