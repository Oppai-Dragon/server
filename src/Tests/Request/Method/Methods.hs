module Tests.Request.Method.Methods
  ( requestMethodMethodsTests
  ) where

import Config
import Data.Request.Method.Methods

import Test.HUnit

requestMethodMethodsTests :: [Test]
requestMethodMethodsTests =
  [TestLabel "isMethodCorrectTest" isMethodCorrectTest]

isMethodCorrectTest :: Test
isMethodCorrectTest =
  TestCase $
  assertEqual "for (isMethodCorrect \"POST\" \"edit\" testApi)" True $
  isMethodCorrect "POST" "edit" testApi
