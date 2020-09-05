module Tests.Request.Access.IsRight
  ( requestAccessIsRightTests
  ) where

import Config

import Data.Request.Access
import Data.Request.Access.IsRight

import Test.HUnit

requestAccessIsRightTests :: [Test]
requestAccessIsRightTests = [TestLabel "isAccessTest" isAccessTest]

isAccessTest :: Test
isAccessTest =
  TestCase $
  assertEqual "for (isAccess \"category\" \"create\" [Admin] testApi)" True $
  isAccess "category" "create" [Admin] testApi
