module Tests.Request.Method.IsRight
  ( requestMethodIsRightTests
  ) where

import Config
import Data.Request.Method.IsRight

import Test.HUnit

requestMethodIsRightTests :: [Test]
requestMethodIsRightTests =
  [TestLabel "isMethodCorrectTest" isMethodCorrectTest]

isMethodCorrectTest :: Test
isMethodCorrectTest =
  TestCase $
  assertEqual "for (isMethodCorrect \"POST\" \"edit\" testApi)" True $
  isMethodCorrect "POST" "edit" testApi
