module Tests.Request.Access.Check
  ( requestAccessCheckTests
  ) where

import Config

import Data.Request.Access
import Data.Request.Access.Check

import Test.HUnit

requestAccessCheckTests :: [Test]
requestAccessCheckTests = [TestLabel "isAccessTest" isAccessTest]

isAccessTest :: Test
isAccessTest =
  TestCase $
  assertEqual "for (isAccess \"category\" \"create\" [Admin] testApi)" True $
  isAccess "category" "create" [Admin] testApi
