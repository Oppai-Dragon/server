module Tests.Request.Access.Methods
  ( requestAccessMethodsTests
  ) where

import Config

import Data.Request.Access
import Data.Request.Access.Methods

import Test.HUnit

requestAccessMethodsTests :: [Test]
requestAccessMethodsTests = [TestLabel "isAccessTest" isAccessTest]

isAccessTest :: Test
isAccessTest =
  TestCase $
  assertEqual "for (isAccess \"category\" \"create\" [Admin] testApi)" True $
  isAccess "category" "create" [Admin] testApi
