module Tests.Essence.GetFields
  ( essenceGetFieldsTests
  ) where

import Data.Essence.GetFields
import Data.Required

import Tests.Essence

import Test.HUnit

essenceGetFieldsTests, getRequiredFields, getEssenceFields :: [Test]
essenceGetFieldsTests = getRequiredFields <> getEssenceFields

getRequiredFields =
  [ TestLabel "iterateRequiredHMTest" iterateRequiredHMTest
  , TestLabel "iterateRequiredHMCreateTest" iterateRequiredHMCreateTest
  , TestLabel "iterateRequiredHMGetTest" iterateRequiredHMGetTest
  , TestLabel "iterateRequiredHMEditTest" iterateRequiredHMEditTest
  , TestLabel "iterateRequiredHMDeleteTest" iterateRequiredHMDeleteTest
  ]

iterateRequiredHMTest, iterateRequiredHMCreateTest, iterateRequiredHMGetTest, iterateRequiredHMEditTest, iterateRequiredHMDeleteTest ::
     Test
iterateRequiredHMTest =
  TestCase $
  assertEqual
    "for (iterateHM testAuthorColumnFieldsString \"create\")"
    ([AND ["person_id"]] :: [Required [String]]) $
  iterateHM testAuthorColumnFieldsString "create"

iterateRequiredHMCreateTest =
  TestCase $
  assertEqual
    "for (iterateHMCreate testAuthorColumnFieldsString)"
    ([AND ["person_id"]] :: [Required [String]]) $
  iterateHMCreate testAuthorColumnFieldsString

iterateRequiredHMGetTest =
  TestCase $
  assertEqual
    "for (iterateHMGet testAuthorColumnFieldsString)"
    ([] :: [Required [String]]) $
  iterateHMGet testAuthorColumnFieldsString

iterateRequiredHMEditTest =
  TestCase $
  assertEqual
    "for (iterateHMEdit testAuthorColumnFieldsString)"
    ([AND ["id"], OR ["person_id"], OR ["description"]] :: [Required [String]]) $
  iterateHMEdit testAuthorColumnFieldsString

iterateRequiredHMDeleteTest =
  TestCase $
  assertEqual
    "for (iterateHMDelete testAuthorColumnFieldsString)"
    ([AND ["id"]] :: [Required [String]]) $
  iterateHMDelete testAuthorColumnFieldsString

getEssenceFields =
  [ TestLabel "iterateEssenceHMTest" iterateEssenceHMTest
  , TestLabel "iterateEssenceHMCreateTest" iterateEssenceHMCreateTest
  , TestLabel "iterateEssenceHMGetTest" iterateEssenceHMGetTest
  , TestLabel "iterateEssenceHMEditTest" iterateEssenceHMEditTest
  , TestLabel "iterateEssenceHMDeleteTest" iterateEssenceHMDeleteTest
  ]

iterateEssenceHMTest, iterateEssenceHMCreateTest, iterateEssenceHMGetTest, iterateEssenceHMEditTest, iterateEssenceHMDeleteTest ::
     Test
iterateEssenceHMTest =
  TestCase $
  assertEqual
    "for (iterateHM testAuthorColumnFieldsString \"create\")"
    ([["person_id"], ["description"]] :: [[String]]) $
  iterateHM testAuthorColumnFieldsString "create"

iterateEssenceHMCreateTest =
  TestCase $
  assertEqual
    "for (iterateHMCreate testAuthorColumnFieldsString)"
    ([["person_id"], ["description"]] :: [[String]]) $
  iterateHMCreate testAuthorColumnFieldsString

iterateEssenceHMGetTest =
  TestCase $
  assertEqual
    "for (iterateHMGet testAuthorColumnFieldsString)"
    ([["id", "person_id", "description"]] :: [[String]]) $
  iterateHMGet testAuthorColumnFieldsString

iterateEssenceHMEditTest =
  TestCase $
  assertEqual
    "for (iterateHMEdit testAuthorColumnFieldsString)"
    ([["id"], ["person_id"], ["description"]] :: [[String]]) $
  iterateHMEdit testAuthorColumnFieldsString

iterateEssenceHMDeleteTest =
  TestCase $
  assertEqual
    "for (iterateHMDelete testAuthorColumnFieldsString)"
    ([["id"]] :: [[String]]) $
  iterateHMDelete testAuthorColumnFieldsString
