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
    "for (iterateHM testAuthorDBFields \"create\")"
    ([AND ["person_id"]] :: [Required [String]]) $
  iterateHM testAuthorDBFields "create"

iterateRequiredHMCreateTest =
  TestCase $
  assertEqual
    "for (iterateHMCreate testAuthorDBFields)"
    ([AND ["person_id"]] :: [Required [String]]) $
  iterateHMCreate testAuthorDBFields

iterateRequiredHMGetTest =
  TestCase $
  assertEqual
    "for (iterateHMGet testAuthorDBFields)"
    ([] :: [Required [String]]) $
  iterateHMGet testAuthorDBFields

iterateRequiredHMEditTest =
  TestCase $
  assertEqual
    "for (iterateHMEdit testAuthorDBFields)"
    ([AND ["id"], OR ["person_id"], OR ["description"]] :: [Required [String]]) $
  iterateHMEdit testAuthorDBFields

iterateRequiredHMDeleteTest =
  TestCase $
  assertEqual
    "for (iterateHMDelete testAuthorDBFields)"
    ([AND ["id"]] :: [Required [String]]) $
  iterateHMDelete testAuthorDBFields

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
    "for (iterateHM testAuthorDBFields \"create\")"
    ([["person_id"], ["description"]] :: [[String]]) $
  iterateHM testAuthorDBFields "create"

iterateEssenceHMCreateTest =
  TestCase $
  assertEqual
    "for (iterateHMCreate testAuthorDBFields)"
    ([["person_id"], ["description"]] :: [[String]]) $
  iterateHMCreate testAuthorDBFields

iterateEssenceHMGetTest =
  TestCase $
  assertEqual "for (iterateHMGet testAuthorDBFields)" ([["id","person_id","description"]] :: [[String]]) $
  iterateHMGet testAuthorDBFields

iterateEssenceHMEditTest =
  TestCase $
  assertEqual
    "for (iterateHMEdit testAuthorDBFields)"
    ([["id"], ["person_id"], ["description"]] :: [[String]]) $
  iterateHMEdit testAuthorDBFields

iterateEssenceHMDeleteTest =
  TestCase $
  assertEqual
    "for (iterateHMDelete testAuthorDBFields)"
    ([["id"]] :: [[String]]) $
  iterateHMDelete testAuthorDBFields
