module Tests.Base.Aeson
  ( baseAesonTests
  ) where

import Data.Base.Aeson
import Prelude hiding (toInteger)

import qualified Data.Aeson as A

import Test.HUnit

baseAesonTests, valueTests, toStrTests :: [Test]
baseAesonTests = valueTests <> toStrTests

valueTests =
  [TestLabel "isNullTest" isNullTest, TestLabel "toIntegerTest" toIntegerTest]

isNullTest, toIntegerTest :: Test
isNullTest = TestCase $ assertEqual "for (isNull Null)" True (isNull A.Null)

toIntegerTest =
  TestCase $
  assertEqual "for (toInteger (A.Number 21321312))" 21321312 $
  toInteger (A.Number 21321312)

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
