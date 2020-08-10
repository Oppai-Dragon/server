module Tests.Required.Methods
    ( requiredMethodsTests
    ) where

import Config
import Data.Essence
import Data.Required
import Data.Required.Methods

import Data.Aeson
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM

import Tests.Essence
import Test.HUnit

requiredMethodsTests =
    [ TestLabel "toFieldsTest"          toFieldsTest
    , TestLabel "getRequiredFieldsTest" getRequiredFieldsTest
    , TestLabel "iterateHMTest"         iterateHMTest
    , TestLabel "iterateHMCreateTest"   iterateHMCreateTest
    , TestLabel "iterateHMGetTest"      iterateHMGetTest
    , TestLabel "iterateHMEditTest"     iterateHMEditTest
    , TestLabel "iterateHMDeleteTest"   iterateHMDeleteTest
    , TestLabel "requiredSequenceATest" requiredSequenceATest
    , TestLabel "requiredApplyTest"           requiredApplyTest
    ]

toFieldsTest =
    TestCase $ assertEqual
    "for (toFields (Required [AND [\"first_name\",\"last_name\"],OR [\"avatar\"]]))"
    (["first_name","last_name","avatar"] :: [String])
    $ toFields (Required [AND ["first_name","last_name"],OR ["avatar"]])

getRequiredFieldsTest =
    TestCase $
    assertEqual "for (getRequiredFields testPersonCreateDB)"
    (Required [AND ["last_name", "first_name"]] :: Required [String])
    $ getRequiredFields testPersonCreateDB testApi

iterateHMTest =
    TestCase $
    assertEqual "for (iterateHM testPersonDBFields \"create\")"
    ([AND ["last_name"],AND ["first_name"]] :: [Required [String]])
    $ iterateHM testPersonDBFields "create"

iterateHMCreateTest =
    TestCase $
    assertEqual "for (iterateHMCreate testPersonDBFields)"
    ([AND ["last_name"], AND ["first_name"]] :: [Required [String]])
    $ iterateHMCreate testPersonDBFields

iterateHMGetTest =
    TestCase $
    assertEqual "for (iterateHMGet testPersonDBFields)"
    ([] :: [Required [String]])
    $ iterateHMGet testPersonDBFields

iterateHMEditTest =
    TestCase $
    assertEqual "for (iterateHMEdit testPersonDBFields)"
    ([AND ["id"],OR ["avatar"],OR ["is_admin"],OR ["last_name"],OR ["first_name"]] :: [Required [String]])
    $ iterateHMEdit testPersonDBFields

iterateHMDeleteTest =
    TestCase $
    assertEqual "for (iterateHMDelete testPersonDBFields)"
    ([AND ["id"]] :: [Required [String]])
    $ iterateHMDelete testPersonDBFields

requiredSequenceATest =
    TestCase $
    assertEqual "for (requiredSequenceA (Required [AND \"id\",AND \"access_key\"]))"
    (AND ["id","access_key"] :: Required [String])
    $ requiredSequenceA [AND ["id"],AND ["access_key"]]

requiredApplyTest =
    TestCase $
    assertEqual "for (requiredApply (AND (\"id\":)) (AND [\"access_key\"]))"
    (AND ["id","access_key"] :: Required [String])
    $ requiredApply (AND (["id"]<>)) (AND ["access_key"])