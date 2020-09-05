module Tests.MyValue
  ( myValueTests
  ) where

import Data.MyValue

import qualified Data.Aeson as A
import qualified Data.Vector as V

import Test.HUnit

myValueTests, fromTests, toTests :: [Test]
myValueTests = fromTests <> toTests

---------------------------------------------------------------------------------
-----------------------------------From------------------------------------------
-----------------------------------Tests-----------------------------------------
---------------------------------------------------------------------------------
fromBSTests, fromValueTests, fromStrTests :: [Test]
fromTests = fromBSTests <> fromValueTests <> fromStrTests

fromBSTests =
  [ TestLabel "fromBSMyIntegersTest" fromBSMyIntegersTest
  , TestLabel "fromBSMyStringsTest" fromBSMyStringsTest
  , TestLabel "fromBSMyBoolTest" fromBSMyBoolTest
  , TestLabel "fromBSMyDateTest" fromBSMyDateTest
  , TestLabel "fromBSMyNextvalTest" fromBSMyNextvalTest
  , TestLabel "fromBSMyIntegerTest" fromBSMyIntegerTest
  , TestLabel "fromBSMyStringTest" fromBSMyStringTest
  , TestLabel "fromBSMyEmptyTest" fromBSMyEmptyTest
  ]

fromBSMyIntegersTest, fromBSMyStringsTest, fromBSMyBoolTest, fromBSMyDateTest, fromBSMyNextvalTest, fromBSMyIntegerTest, fromBSMyStringTest, fromBSMyEmptyTest ::
     Test
fromBSMyIntegersTest =
  TestCase $
  assertEqual "for (fromBS \"[1,2]\")" (MyIntegers [1, 2]) $ fromBS "[1,2]"

fromBSMyStringsTest =
  TestCase $
  assertEqual "for (fromBS \"[\"k1\",\"k2\"]\")" (MyStrings ["k1", "k2"]) $
  fromBS "[\"k1\",\"k2\"]"

fromBSMyBoolTest =
  TestCase $
  assertEqual "for (fromBS \"false\")" (MyBool False) $ fromBS "false"

fromBSMyDateTest =
  TestCase $
  assertEqual "for (fromBS \"2020-07-01\")" (MyDate "2020-07-01") $
  fromBS "2020-07-01"

fromBSMyNextvalTest =
  TestCase $
  assertEqual
    "for (fromBS \"nextval(person_id_seq)\")"
    (MyNextval "nextval(person_id_seq)") $
  fromBS "nextval(person_id_seq)"

fromBSMyIntegerTest =
  TestCase $ assertEqual "for (fromBS \"1\")" (MyInteger 1) $ fromBS "1"

fromBSMyStringTest =
  TestCase $ assertEqual "for (fromBS \"kek\")" (MyString "kek") $ fromBS "kek"

fromBSMyEmptyTest =
  TestCase $ assertEqual "for (fromBS \"\")" MyEmpty $ fromBS ""

fromValueTests =
  [ TestLabel "fromValueMyIntegersTest" fromValueMyIntegersTest
  , TestLabel "fromValueMyStringsTest" fromValueMyStringsTest
  , TestLabel "fromValueMyBoolTest" fromValueMyBoolTest
  , TestLabel "fromValueMyDateTest" fromValueMyDateTest
  , TestLabel "fromValueMyNextValTest" fromValueMyNextvalTest
  , TestLabel "fromValueMyIntegerTest" fromValueMyIntegerTest
  , TestLabel "fromValueMyStringTest" fromValueMyStringTest
  , TestLabel "fromValueMyEmptyTest" fromValueMyEmptyTest
  ]

fromValueMyIntegersTest, fromValueMyStringsTest, fromValueMyBoolTest, fromValueMyDateTest, fromValueMyNextvalTest, fromValueMyIntegerTest, fromValueMyStringTest, fromValueMyEmptyTest ::
     Test
fromValueMyIntegersTest =
  TestCase $
  assertEqual
    "for (fromValue (Array $ V.fromList [Number 1,Number 2]))"
    (MyIntegers [1, 2]) $
  fromValue (A.Array $ V.fromList [A.Number 1, A.Number 2])

fromValueMyStringsTest =
  TestCase $
  assertEqual
    "for (fromValue (Array $ V.fromList [String \"k1\",String \"k2\"]))"
    (MyStrings ["k1", "k2"]) $
  fromValue (A.Array $ V.fromList [A.String "k1", A.String "k2"])

fromValueMyBoolTest =
  TestCase $
  assertEqual "for (fromValue (Bool False))" (MyBool False) $
  fromValue (A.Bool False)

fromValueMyDateTest =
  TestCase $
  assertEqual "for (fromValue (String \"2020-07-01\"))" (MyDate "2020-07-01") $
  fromValue (A.String "2020-07-01")

fromValueMyNextvalTest =
  TestCase $
  assertEqual
    "for (fromValue (String \"nextval(person_id_seq)\"))"
    (MyNextval "nextval(person_id_seq)") $
  fromValue (A.String "nextval(person_id_seq)")

fromValueMyIntegerTest =
  TestCase $
  assertEqual "for (fromValue (Number 1))" (MyInteger 1) $
  fromValue (A.Number 1)

fromValueMyStringTest =
  TestCase $
  assertEqual "for (fromValue (String \"kek\"))" (MyString "kek") $
  fromValue (A.String "kek")

fromValueMyEmptyTest =
  TestCase $ assertEqual "for (fromValue Null)" MyEmpty $ fromValue A.Null

fromStrTests =
  [ TestLabel "fromStrMyIntegersTest" fromStrMyIntegersTest
  , TestLabel "fromStrMyStringsTest" fromStrMyStringsTest
  , TestLabel "fromStrMyBoolTest" fromStrMyBoolTest
  , TestLabel "fromStrMyDateTest" fromStrMyDateTest
  , TestLabel "fromStrMyNextvalTest" fromStrMyNextvalTest
  , TestLabel "fromStrMyIntegerTest" fromStrMyIntegerTest
  , TestLabel "fromStrMyStringTest" fromStrMyStringTest
  , TestLabel "fromStrMyEmptyTest" fromStrMyEmptyTest
  ]

fromStrMyIntegersTest, fromStrMyStringsTest, fromStrMyBoolTest, fromStrMyDateTest, fromStrMyNextvalTest, fromStrMyIntegerTest, fromStrMyStringTest, fromStrMyEmptyTest ::
     Test
fromStrMyIntegersTest =
  TestCase $
  assertEqual "for (fromStr \"[1,2]\")" (MyIntegers [1, 2]) $ fromStr "[1,2]"

fromStrMyStringsTest =
  TestCase $
  assertEqual "for (fromStr \"[\"k1\",\"k2\"]\")" (MyStrings ["k1", "k2"]) $
  fromStr "[\"k1\",\"k2\"]"

fromStrMyBoolTest =
  TestCase $
  assertEqual "for (fromStr \"false\")" (MyBool False) $ fromStr "false"

fromStrMyDateTest =
  TestCase $
  assertEqual "for (fromStr \"2020-07-01\")" (MyDate "2020-07-01") $
  fromStr "2020-07-01"

fromStrMyNextvalTest =
  TestCase $
  assertEqual
    "for (fromStr \"nextval(person_id_seq)\")"
    (MyNextval "nextval(person_id_seq)") $
  fromStr "nextval(person_id_seq)"

fromStrMyIntegerTest =
  TestCase $ assertEqual "for (fromStr \"1\")" (MyInteger 1) $ fromStr "1"

fromStrMyStringTest =
  TestCase $
  assertEqual "for (fromStr \"kek\")" (MyString "kek") $ fromStr "kek"

fromStrMyEmptyTest =
  TestCase $ assertEqual "for (fromStr \"\")" MyEmpty $ fromStr ""

---------------------------------------------------------------------------------
------------------------------------To-------------------------------------------
-----------------------------------Tests-----------------------------------------
---------------------------------------------------------------------------------
toStrTests, toValueTests :: [Test]
toTests = toStrTests <> toValueTests

toStrTests =
  [ TestLabel "myIntegerToStrTest" myIntegerToStrTest
  , TestLabel "myStringToStrTest" myStringToStrTest
  , TestLabel "myBoolToStrTest" myBoolToStrTest
  , TestLabel "myIntegersToStrTest" myIntegersToStrTest
  , TestLabel "myStringsToStrTest" myStringsToStrTest
  , TestLabel "myNextValToStrTest" myNextValToStrTest
  , TestLabel "myDateToStrTest" myDateToStrTest
  , TestLabel "myEmptyToStrTest" myEmptyToStrTest
  ]

myIntegerToStrTest, myStringToStrTest, myBoolToStrTest, myIntegersToStrTest, myStringsToStrTest, myNextValToStrTest, myDateToStrTest, myEmptyToStrTest ::
     Test
myIntegerToStrTest =
  TestCase $ assertEqual "for (toStr (MyInteger 1))" "1" $ toStr (MyInteger 1)

myStringToStrTest =
  TestCase $
  assertEqual "for (toStr (MyString \"kek\"))" "kek" $ toStr (MyString "kek")

myBoolToStrTest =
  TestCase $
  assertEqual "for (toStr (MyBool False))" "False" $ toStr (MyBool False)

myIntegersToStrTest =
  TestCase $
  assertEqual "for (toStr (MyIntegers [1,2]))" "[1,2]" $
  toStr (MyIntegers [1, 2])

myStringsToStrTest =
  TestCase $
  assertEqual "for (toStr (MyStrings [\"k1\",\"k2\"]))" "[k1,k2]" $
  toStr (MyStrings ["k1", "k2"])

myNextValToStrTest =
  TestCase $
  assertEqual "for (toStr (MyNextval \"kek\"))" "kek" $ toStr (MyNextval "kek")

myDateToStrTest =
  TestCase $
  assertEqual "for (toStr (MyDate \"2020-07-02\"))" "2020-07-02" $
  toStr (MyDate "2020-07-02")

myEmptyToStrTest =
  TestCase $ assertEqual "for (toStr MyEmpty)" "" $ toStr MyEmpty

toValueTests =
  [ TestLabel "myIntegerToValueTest" myIntegerToValueTest
  , TestLabel "myStringToValueTest" myStringToValueTest
  , TestLabel "myBoolToValueTest" myBoolToValueTest
  , TestLabel "myIntegersToValueTest" myIntegersToValueTest
  , TestLabel "myStringsToValueTest" myStringsToValueTest
  , TestLabel "myNextValToValueTest" myNextValToValueTest
  , TestLabel "myDateToValueTest" myDateToValueTest
  , TestLabel "myEmptyToValueTest" myEmptyToValueTest
  ]

myIntegerToValueTest, myStringToValueTest, myBoolToValueTest, myIntegersToValueTest, myStringsToValueTest, myNextValToValueTest, myDateToValueTest, myEmptyToValueTest ::
     Test
myIntegerToValueTest =
  TestCase $
  assertEqual "for (toValue (MyInteger 1))" (A.Number 1) $ toValue (MyInteger 1)

myStringToValueTest =
  TestCase $
  assertEqual "for (toValue (MyString \"kek\"))" (A.String "kek") $
  toValue (MyString "kek")

myBoolToValueTest =
  TestCase $
  assertEqual "for (toValue (MyBool False))" (A.Bool False) $
  toValue (MyBool False)

myIntegersToValueTest =
  TestCase $
  assertEqual
    "for (toValue (MyIntegers [1,2]))"
    (A.Array $ V.fromList [A.Number 1, A.Number 2]) $
  toValue (MyIntegers [1, 2])

myStringsToValueTest =
  TestCase $
  assertEqual
    "for (toValue (MyStrings [\"k1\",\"k2\"]))"
    (A.Array $ V.fromList [A.String "k1", A.String "k2"]) $
  toValue (MyStrings ["k1", "k2"])

myNextValToValueTest =
  TestCase $
  assertEqual "for (toValue (MyNextval \"kek\"))" (A.String "kek") $
  toValue (MyNextval "kek")

myDateToValueTest =
  TestCase $
  assertEqual "for (toValue (MyDate \"2020-07-02\"))" (A.String "2020-07-02") $
  toValue (MyDate "2020-07-02")

myEmptyToValueTest =
  TestCase $ assertEqual "for (toValue MyEmpty)" A.Null $ toValue MyEmpty
