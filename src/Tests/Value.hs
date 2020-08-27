module Tests.Value
  ( valueTests
  ) where

import Data.Value

import qualified Data.Aeson as A
import qualified Data.Vector as V

import Test.HUnit

valueTests, isValueTests, toTextArrTests, toStrArrTests, toTextTests, toStrTests ::
     [Test]
valueTests =
  isValueTests <> toTextArrTests <> toStrArrTests <> toTextTests <> toStrTests

isValueTests = [TestLabel "isNullTest" isNullTest]

isNullTest :: Test
isNullTest = TestCase $ assertEqual "for (isNull Null)" True (isNull A.Null)

toTextArrTests =
  [ TestLabel "arrayToTextArrTest" arrayToTextArrTest
  , TestLabel "objectToTextArrTest" objectToTextArrTest
  , TestLabel "nullToTextArrTest" nullToTextArrTest
  , TestLabel "numberToTextArrTest" numberToTextArrTest
  , TestLabel "stringToTextArrTest" stringToTextArrTest
  , TestLabel "boolToTextArrTest" boolToTextArrTest
  ]

arrayToTextArrTest, objectToTextArrTest, nullToTextArrTest, numberToTextArrTest, stringToTextArrTest, boolToTextArrTest ::
     Test
arrayToTextArrTest =
  TestCase .
  assertEqual "for (toTextArr (Array $ V.singleton $ String \"kek\"))" ["kek"] .
  toTextArr . A.Array . V.singleton $
  A.String "kek"

objectToTextArrTest =
  TestCase $
  assertEqual "for (toTextArr (object [\"kek\" .= String \"kok\"]))" ["kek"] $
  toTextArr (A.object ["kek" A..= A.String "kok"])

nullToTextArrTest =
  TestCase $ assertEqual "for (toTextArr Null)" [] $ toTextArr A.Null

numberToTextArrTest =
  TestCase $
  assertEqual "for (toTextArr (Number 1))" ["1"] $ toTextArr (A.Number 1)

stringToTextArrTest =
  TestCase $
  assertEqual "for (toTextArr (String \"kek\"))" ["kek"] $
  toTextArr (A.String "kek")

boolToTextArrTest =
  TestCase $
  assertEqual "for (toTextArr (Bool True))" ["True"] $ toTextArr (A.Bool True)

toStrArrTests =
  [ TestLabel "arrayToStrArrTest" arrayToStrArrTest
  , TestLabel "objectToStrArrTest" objectToStrArrTest
  , TestLabel "nullToStrArrTest" nullToStrArrTest
  , TestLabel "numberToStrArrTest" numberToStrArrTest
  , TestLabel "stringToStrArrTest" stringToStrArrTest
  , TestLabel "boolToStrArrTest" boolToStrArrTest
  ]

arrayToStrArrTest, objectToStrArrTest, nullToStrArrTest, numberToStrArrTest, stringToStrArrTest, boolToStrArrTest ::
     Test
arrayToStrArrTest =
  TestCase $
  assertEqual "for (toStrArr (Array $ V.singleton $ String \"kek\"))" ["kek"] $
  toStrArr (A.Array . V.singleton $ A.String "kek")

objectToStrArrTest =
  TestCase $
  assertEqual "for (toStrArr (object [\"kek\" .= String \"kok\"]))" ["kek"] $
  toStrArr (A.object ["kek" A..= A.String "kok"])

nullToStrArrTest =
  TestCase $ assertEqual "for (toStrArr Null)" [] $ toStrArr A.Null

numberToStrArrTest =
  TestCase $
  assertEqual "for (toStrArr (Number 1))" ["1"] $ toStrArr (A.Number 1)

stringToStrArrTest =
  TestCase $
  assertEqual "for (toStrArr (String \"kek\"))" ["kek"] $
  toStrArr (A.String "kek")

boolToStrArrTest =
  TestCase $
  assertEqual "for (toStrArr (Bool True))" ["True"] $ toStrArr (A.Bool True)

toTextTests =
  [ TestLabel "numberToTextTest" numberToTextTest
  , TestLabel "stringToTextTest" stringToTextTest
  , TestLabel "boolToTextTest" boolToTextTest
  , TestLabel "othersToTextTest" othersToTextTest
  ]

numberToTextTest, stringToTextTest, boolToTextTest, othersToTextTest :: Test
numberToTextTest =
  TestCase $ assertEqual "for (toText (Number 1))" "1" $ toText (A.Number 1)

stringToTextTest =
  TestCase $
  assertEqual "for (toText (String \"kek\"))" "kek" $ toText (A.String "kek")

boolToTextTest =
  TestCase $
  assertEqual "for (toText (Bool False))" "False" $ toText (A.Bool False)

othersToTextTest = TestCase $ assertEqual "for (toText Null)" "" $ toText A.Null

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
