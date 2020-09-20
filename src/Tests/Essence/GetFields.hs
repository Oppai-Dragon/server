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
    "for (iterateHM testAuthorColumnFields \"create\")"
    ([AND ["person_id"]] :: [Required [String]]) $
  iterateHM testAuthorColumnFields "create"

iterateRequiredHMCreateTest =
  TestCase $
  assertEqual
    "for (iterateHMCreate testAuthorColumnFields)"
    ([AND ["person_id"]] :: [Required [String]]) $
  iterateHMCreate testAuthorColumnFields

iterateRequiredHMGetTest =
  TestCase $
  assertEqual
    "for (iterateHMGet testAuthorColumnFields)"
    ([] :: [Required [String]]) $
  iterateHMGet testAuthorColumnFields

iterateRequiredHMEditTest =
  TestCase $
  assertEqual
    "for (iterateHMEdit testAuthorColumnFields)"
    ([AND ["id"], OR ["person_id"], OR ["description"]] :: [Required [String]]) $
  iterateHMEdit testAuthorColumnFields

iterateRequiredHMDeleteTest =
  TestCase $
  assertEqual
    "for (iterateHMDelete testAuthorColumnFields)"
    ([AND ["id"]] :: [Required [String]]) $
  iterateHMDelete testAuthorColumnFields

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
    "for (iterateHM testAuthorColumnFields \"create\")"
    ([["person_id"], ["description"]] :: [[String]]) $
  iterateHM testAuthorColumnFields "create"

iterateEssenceHMCreateTest =
  TestCase $
  assertEqual
    "for (iterateHMCreate testAuthorColumnFields)"
    ([["person_id"], ["description"]] :: [[String]]) $
  iterateHMCreate testAuthorColumnFields

iterateEssenceHMGetTest =
  TestCase $
  assertEqual
    "for (iterateHMGet testAuthorColumnFields)"
    ([["id", "person_id", "description"]] :: [[String]]) $
  iterateHMGet testAuthorColumnFields

iterateEssenceHMEditTest =
  TestCase $
  assertEqual
    "for (iterateHMEdit testAuthorColumnFields)"
    ([["id"], ["person_id"], ["description"]] :: [[String]]) $
  iterateHMEdit testAuthorColumnFields

iterateEssenceHMDeleteTest =
  TestCase $
  assertEqual
    "for (iterateHMDelete testAuthorColumnFields)"
    ([["id"]] :: [[String]]) $
  iterateHMDelete testAuthorColumnFields
