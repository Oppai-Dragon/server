module Tests.Request.Method.Check
  ( requestMethodCheckTests
  ) where

import Config
import Data.Request.Method.Check

import Test.HUnit

requestMethodCheckTests :: [Test]
requestMethodCheckTests =
  [TestLabel "isMethodCorrectTest" isMethodCorrectTest]

isMethodCorrectTest :: Test
isMethodCorrectTest =
  TestCase $
  assertEqual "for (isMethodCorrect \"POST\" \"edit\" testApi)" True $
  isMethodCorrect "POST" "edit" testApi
