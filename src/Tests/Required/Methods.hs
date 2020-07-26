module Tests.Required.Methods
    ( requiredMethodsTests
    ) where

import Data.Essence
import Data.MyValue
import Data.Required
import Data.Required.Methods

import Data.Aeson
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM

import Test.HUnit

requiredMethodsTests =
    [ TestLabel "getRequiredFieldsTest" getRequiredFieldsTest
    , TestLabel "iterateHMTest"         iterateHMTest
    , TestLabel "iterateHMCreateTest"   iterateHMCreateTest
    , TestLabel "iterateHMGetTest"      iterateHMGetTest
    , TestLabel "iterateHMEditTest"     iterateHMEditTest
    , TestLabel "iterateHMDeleteTest"   iterateHMDeleteTest
    , TestLabel "mySequenceATest"       mySequenceATest
    , TestLabel "myApplyTest"           myApplyTest
    ]

getRequiredFieldsTest =
    TestCase $
    assertEqual "for (getRequiredFields testEssenceDB)"
    (Required [AND ["last_name", "first_name"]] :: Required [String])
    $ getRequiredFields testEssenceDB

iterateHMTest =
    TestCase $
    assertEqual "for (iterateHM testEssenceDBFields \"create\")"
    ([AND "first_name", AND "last_name"] :: [Required String])
    $ iterateHM testEssenceDBFields "create"

iterateHMCreateTest =
    TestCase $
    assertEqual "for (iterateHMCreate testEssenceDBFields)"
    ([AND "first_name", AND "last_name"] :: [Required String])
    $ iterateHMCreate testEssenceDBFields

iterateHMGetTest =
    TestCase $
    assertEqual "for (iterateHMGet testEssenceDBFields)"
    []
    $ iterateHMGet testEssenceDBFields

iterateHMEditTest =
    TestCase $
    assertEqual "for (iterateHMEdit testEssenceDBFields)"
    ([AND "id",OR "first_name",OR "last_name",OR "avatar",OR "is_admin",AND "access_key"] :: [Required String])
    $ iterateHMEdit testEssenceDBFields

iterateHMDeleteTest =
    TestCase $
    assertEqual "for (iterateHMDelete testEssenceDBFields)"
    ([AND "id",AND "access_key"] :: [Required String])
    $ iterateHMDelete testEssenceDBFields

mySequenceATest =
    TestCase $
    assertEqual "for (mySequenceA (Required [AND \"id\",AND \"access_key\"]))"
    (AND ["id","access_key"] :: Required [String])
    $ mySequenceA [AND "id",AND "access_key"]

myApplyTest =
    TestCase $
    assertEqual "for (myApply (AND (\"id\":)) (AND [\"access_key\"]))"
    (AND ["id","access_key"] :: Required [String])
    $ myApply (AND ("id":)) (AND ["access_key"])

testEssenceDB = EssenceDB "person" "create" $ HM.fromList testEssenceDBFields

testEssenceDBFields =
    [("id", Description (MyNextval "") (Just $ NOT NULL) Nothing Nothing)
    ,("first_name", Description (MyString "") (Just $ NOT NULL) Nothing Nothing)
    ,("last_name", Description (MyString "") (Just $ NOT NULL) Nothing Nothing)
    ,("date_of_creation", Description (MyString "") (Just $ NOT NULL) Nothing Nothing)
    ,("avatar", Description (MyString "") (Just NULL) Nothing Nothing)
    ,("is_admin", Description (MyBool False) (Just NULL) Nothing Nothing)
    ,("access_key", Description (MyString "") (Just NULL) Nothing Nothing)
    ]