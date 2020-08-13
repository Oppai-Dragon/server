module Tests.Request.Params.Methods
    ( requestParamsMethodsTests
    ) where

import Config
import Data.Essence
import Data.MyValue
import Data.Required
import Data.Request.Params.Methods

import Data.Monoid

import Tests.Essence

import Test.HUnit

requestParamsMethodsTests =
    [ TestLabel "isRequiredParamsTest"          isRequiredParamsTest
    , TestLabel "iterateRequiredParamsTest"     iterateRequiredParamsTest
    , TestLabel "iterateParamsTest"             iterateParamsTest
    , TestLabel "queryBSWithoutMaybeTest"       queryBSWithoutMaybeTest
    , TestLabel "isTypeParamsCorrectTest"       isTypeParamsCorrectTest
    , TestLabel "compareValueTypeTest"          compareValueTypeTest
    ]

isRequiredParamsTest =
    TestCase $
    assertEqual
    "for (isRequiredParams testPersonCreateDB [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")])"
    True
    $ isRequiredParams testPersonCreateDB [("first_name",Just "misha"),("last_name",Just "dragon")] testApi

iterateRequiredParamsTest =
    TestCase $
    assertEqual
    "for (iterateRequiredParams (Required [AND [\"first_name\",\"last_name\"]]) [(\"first_name\",\"misha\"),(\"last_name\",\"dragon\")])"
    True $ iterateRequiredParams
    (Required [AND ["first_name","last_name"]])
    [("first_name","misha"),("last_name","dragon")]

iterateParamsTest =
    TestCase $
    assertEqual
    "for (iterateParams [\"first_name\",\"last_name\"] [(\"first_name\",\"misha\"),(\"last_name\",\"dragon\")])"
    [True,True]
    $ iterateParams ["first_name","last_name"]
    [("first_name","misha"),("last_name","dragon")]

queryBSWithoutMaybeTest =
    TestCase $
    assertEqual
    "for (queryBSWithoutMaybe [(\"first_name\",Just \"misha\"),(\"last_name\",Just \"dragon\")] [(\"first_name\",\"misha\"),(\"last_name\",\"dragon\")])"
    [("first_name","misha"),("last_name","dragon")]
    $ queryBSWithoutMaybe [("first_name",Just "misha"),("last_name",Just "dragon")]

isTypeParamsCorrectTest =
    TestCase $
    assertEqual
    "for (isTypeParamsCorrect testPersonCreateDB [(\"first_name\",MyString \"misha\"),(\"last_name\",MyString \"dragon\")])"
    (All True)
    $ isTypeParamsCorrect testPersonCreateDB
    [("first_name",MyString "misha"),("last_name",MyString "dragon")]

compareValueTypeTest =
    TestCase $
    assertEqual "for (compareValueType (MyInteger 1) (MyInteger 2))"
    True
    $ compareValueType (MyInteger 1) (MyInteger 2)
