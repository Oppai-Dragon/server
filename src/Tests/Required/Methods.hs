module Tests.Required.Methods
    ( requiredMethodsTests
    ) where

import Data.Essence
import Data.Required
import Data.Required.Methods

import Data.Aeson
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM

import Tests.Essence
import Test.HUnit

requiredMethodsTests =
    [ TestLabel "getRequiredFieldsTest" getRequiredFieldsTest
    , TestLabel "iterateHMTest"         iterateHMTest
    , TestLabel "iterateHMCreateTest"   iterateHMCreateTest
    , TestLabel "iterateHMGetTest"      iterateHMGetTest
    , TestLabel "iterateHMEditTest"     iterateHMEditTest
    , TestLabel "iterateHMDeleteTest"   iterateHMDeleteTest
    , TestLabel "requiredSequenceATest" requiredSequenceATest
    , TestLabel "requiredApplyTest"           requiredApplyTest
    ]

getRequiredFieldsTest =
    TestCase $
    assertEqual "for (getRequiredFields testEssenceDB)"
    (Required [AND ["last_name", "first_name"]] :: Required [String])
    $ getRequiredFields testEssenceDB

iterateHMTest =
    TestCase $
    assertEqual "for (iterateHM testEssenceDBFields \"create\")"
    ([AND "last_name",AND "first_name"] :: [Required String])
    $ iterateHM testEssenceDBFields "create"

iterateHMCreateTest =
    TestCase $
    assertEqual "for (iterateHMCreate testEssenceDBFields)"
    ([AND "last_name", AND "first_name"] :: [Required String])
    $ iterateHMCreate testEssenceDBFields

iterateHMGetTest =
    TestCase $
    assertEqual "for (iterateHMGet testEssenceDBFields)"
    []
    $ iterateHMGet testEssenceDBFields

iterateHMEditTest =
    TestCase $
    assertEqual "for (iterateHMEdit testEssenceDBFields)"
    ([AND "id",OR "avatar",OR "is_admin",OR "last_name",OR "first_name"] :: [Required String])
    $ iterateHMEdit testEssenceDBFields

iterateHMDeleteTest =
    TestCase $
    assertEqual "for (iterateHMDelete testEssenceDBFields)"
    ([AND "id"] :: [Required String])
    $ iterateHMDelete testEssenceDBFields

requiredSequenceATest =
    TestCase $
    assertEqual "for (requiredSequenceA (Required [AND \"id\",AND \"access_key\"]))"
    (AND ["id","access_key"] :: Required [String])
    $ requiredSequenceA [AND "id",AND "access_key"]

requiredApplyTest =
    TestCase $
    assertEqual "for (requiredApply (AND (\"id\":)) (AND [\"access_key\"]))"
    (AND ["id","access_key"] :: Required [String])
    $ requiredApply (AND ("id":)) (AND ["access_key"])