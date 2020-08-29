module Tests.Value
  ( valueTests
  ) where

import Data.Value

import qualified Data.Aeson as A

import Test.HUnit

valueTests, isValueTests, toStrTests :: [Test]
valueTests = isValueTests <> toStrTests

isValueTests = [TestLabel "isNullTest" isNullTest]

isNullTest :: Test
isNullTest = TestCase $ assertEqual "for (isNull Null)" True (isNull A.Null)

toStrTests =
  [ TestLabel "numberToStrTest" numberToStrTest
  , TestLabel "stringToStrTest" stringToStrTest
  , TestLabel "boolToStrTest" boolToStrTest
  , TestLabel "othersToStrTest" othersToStrTest
  ]

numberToStrTest, stringToStrTest, boolToStrTest, othersToStrTest :: Test
numberToStrTest =
  TestCase $ assertEqual "for (toStr (Number 1))" "1" $ toStr (A.Number 1)

stringToStrTest =
  TestCase $
  assertEqual "for (toStr (String \"kek\"))" "kek" $ toStr (A.String "kek")

boolToStrTest =
  TestCase $
  assertEqual "for (toStr (Bool False))" "False" $ toStr (A.Bool False)

othersToStrTest = TestCase $ assertEqual "for (toStr Null)" "" $ toStr A.Null
