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
fromValueTests, fromStrTests :: [Test]
fromTests = fromValueTests <> fromStrTests

fromValueTests =
  [ TestLabel "fromValueMyIntegerArrTest" fromValueMyIntegerArrTest
  , TestLabel "fromValueMyStringArrTest" fromValueMyStringArrTest
  , TestLabel "fromValueMyBoolTest" fromValueMyBoolTest
  , TestLabel "fromValueMyBoolArrTest" fromValueMyBoolArrTest
  , TestLabel "fromValueMyDateTest" fromValueMyDateTest
  , TestLabel "fromValueMyNextValTest" fromValueMyNextvalTest
  , TestLabel "fromValueMyIntegerTest" fromValueMyIntegerTest
  , TestLabel "fromValueMyStringTest" fromValueMyStringTest
  , TestLabel "fromValueMyEmptyTest" fromValueMyEmptyTest
  ]

fromValueMyIntegerArrTest, fromValueMyStringArrTest, fromValueMyBoolTest, fromValueMyBoolArrTest, fromValueMyDateTest, fromValueMyNextvalTest, fromValueMyIntegerTest, fromValueMyStringTest, fromValueMyEmptyTest ::
     Test
fromValueMyIntegerArrTest =
  TestCase $
  assertEqual
    "for (fromValue (Array $ V.fromList [Number 1,Number 2]))"
    (MyIntegerArr [1, 2]) $
  fromValue (A.Array $ V.fromList [A.Number 1, A.Number 2])

fromValueMyStringArrTest =
  TestCase $
  assertEqual
    "for (fromValue (Array $ V.fromList [String \"k1\",String \"k2\"]))"
    (MyStringArr ["k1", "k2"]) $
  fromValue (A.Array $ V.fromList [A.String "k1", A.String "k2"])

fromValueMyBoolTest =
  TestCase $
  assertEqual "for (fromValue (Bool False))" (MyBool False) $
  fromValue (A.Bool False)

fromValueMyBoolArrTest =
  TestCase $
  assertEqual
    "for (fromValue (Array $ V.fromList [Bool False,Bool True]))"
    (MyBoolArr [False, True]) $
  fromValue (A.Array $ V.fromList [A.Bool False, A.Bool True])

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
  [ TestLabel "fromStrMyIntegerArrTest" fromStrMyIntegerArrTest
  , TestLabel "fromStrMyStringArrTest" fromStrMyStringArrTest
  , TestLabel "fromStrMyBoolTest" fromStrMyBoolTest
  , TestLabel "fromStrMyBoolArrTest" fromStrMyBoolArrTest
  , TestLabel "fromStrMyDateTest" fromStrMyDateTest
  , TestLabel "fromStrMyDateArrTest" fromStrMyDateArrTest
  , TestLabel "fromStrMyUriTest" fromStrMyUriTest
  , TestLabel "fromStrMyNextvalTest" fromStrMyNextvalTest
  , TestLabel "fromStrMyIntegerTest" fromStrMyIntegerTest
  , TestLabel "fromStrMyStringTest" fromStrMyStringTest
  , TestLabel "fromStrMyEmptyTest" fromStrMyEmptyTest
  ]

fromStrMyIntegerArrTest, fromStrMyStringArrTest, fromStrMyBoolTest, fromStrMyBoolArrTest, fromStrMyDateTest, fromStrMyDateArrTest, fromStrMyUriTest, fromStrMyNextvalTest, fromStrMyIntegerTest, fromStrMyStringTest, fromStrMyEmptyTest ::
     Test
fromStrMyIntegerArrTest =
  TestCase $
  assertEqual "for (fromStr \"[1,2]\")" (MyIntegerArr [1, 2]) $ fromStr "[1,2]"

fromStrMyStringArrTest =
  TestCase $
  assertEqual "for (fromStr \"[\"k1\",\"k2\"]\")" (MyStringArr ["k1", "k2"]) $
  fromStr "[\"k1\",\"k2\"]"

fromStrMyBoolTest =
  TestCase $
  assertEqual "for (fromStr \"false\")" (MyBool False) $ fromStr "false"

fromStrMyBoolArrTest =
  TestCase $
  assertEqual "for (fromStr \"[false,true]\")" (MyBoolArr [False, True]) $
  fromStr "[false,true]"

fromStrMyDateTest =
  TestCase $
  assertEqual "for (fromStr \"2020-07-01\")" (MyDate "2020-07-01") $
  fromStr "2020-07-01"

fromStrMyDateArrTest =
  TestCase $
  assertEqual
    "for (fromStr \"[2020-07-01,2020-07-01]\")"
    (MyDateArr ["2020-07-01", "2020-07-01"]) $
  fromStr "[2020-07-01,2020-07-01]"

fromStrMyUriTest =
  TestCase $
  assertEqual
    "for (fromStr \"https://priziva.net/gor/\")"
    (MyUri "https://priziva.net/gor/") $
  fromStr "https://priziva.net/gor/"

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
  , TestLabel "myBoolArrToStrTest" myBoolArrToStrTest
  , TestLabel "myIntegerArrToStrTest" myIntegerArrToStrTest
  , TestLabel "myStringArrToStrTest" myStringArrToStrTest
  , TestLabel "myNextValToStrTest" myNextValToStrTest
  , TestLabel "myDateToStrTest" myDateToStrTest
  , TestLabel "myDateArrToStrTest" myDateArrToStrTest
  , TestLabel "myUriToStrTest" myUriToStrTest
  , TestLabel "myEmptyToStrTest" myEmptyToStrTest
  ]

myIntegerToStrTest, myStringToStrTest, myBoolToStrTest, myBoolArrToStrTest, myIntegerArrToStrTest, myStringArrToStrTest, myNextValToStrTest, myDateToStrTest, myDateArrToStrTest, myUriToStrTest, myEmptyToStrTest ::
     Test
myIntegerToStrTest =
  TestCase $ assertEqual "for (toStr (MyInteger 1))" "1" $ toStr (MyInteger 1)

myStringToStrTest =
  TestCase $
  assertEqual "for (toStr (MyString \"kek\"))" "kek" $ toStr (MyString "kek")

myBoolToStrTest =
  TestCase $
  assertEqual "for (toStr (MyBool False))" "False" $ toStr (MyBool False)

myBoolArrToStrTest =
  TestCase $
  assertEqual "for (toStr (MyBoolArr [False,True]))" "[False,True]" $
  toStr (MyBoolArr [False, True])

myIntegerArrToStrTest =
  TestCase $
  assertEqual "for (toStr (MyIntegers [1,2]))" "[1,2]" $
  toStr (MyIntegerArr [1, 2])

myStringArrToStrTest =
  TestCase $
  assertEqual "for (toStr (MyStrings [\"k1\",\"k2\"]))" "[k1,k2]" $
  toStr (MyStringArr ["k1", "k2"])

myNextValToStrTest =
  TestCase $
  assertEqual "for (toStr (MyNextval \"kek\"))" "kek" $ toStr (MyNextval "kek")

myDateToStrTest =
  TestCase $
  assertEqual "for (toStr (MyDate \"2020-07-02\"))" "2020-07-02" $
  toStr (MyDate "2020-07-02")

myDateArrToStrTest =
  TestCase $
  assertEqual
    "for (toStr (MyDateArr [\"2020-07-02\",\"2020-07-02\"]))"
    "[2020-07-02,2020-07-02]" $
  toStr (MyDateArr ["2020-07-02", "2020-07-02"])

myUriToStrTest =
  TestCase $
  assertEqual
    "for (toStr (MyUri \"https://priziva.net/gor/\"))"
    "https://priziva.net/gor/" $
  toStr (MyUri "https://priziva.net/gor/")

myEmptyToStrTest =
  TestCase $ assertEqual "for (toStr MyEmpty)" "" $ toStr MyEmpty

toValueTests =
  [ TestLabel "myIntegerToValueTest" myIntegerToValueTest
  , TestLabel "myStringToValueTest" myStringToValueTest
  , TestLabel "myBoolToValueTest" myBoolToValueTest
  , TestLabel "myBoolArrToValueTest" myBoolArrToValueTest
  , TestLabel "myIntegerArrToValueTest" myIntegerArrToValueTest
  , TestLabel "myStringArrToValueTest" myStringArrToValueTest
  , TestLabel "myNextValToValueTest" myNextValToValueTest
  , TestLabel "myDateToValueTest" myDateToValueTest
  , TestLabel "myDateArrToValueTest" myDateArrToValueTest
  , TestLabel "myUriToValueTest" myUriToValueTest
  , TestLabel "myEmptyToValueTest" myEmptyToValueTest
  ]

myIntegerToValueTest, myStringToValueTest, myBoolToValueTest, myBoolArrToValueTest, myIntegerArrToValueTest, myStringArrToValueTest, myNextValToValueTest, myDateToValueTest, myDateArrToValueTest, myUriToValueTest, myEmptyToValueTest ::
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

myBoolArrToValueTest =
  TestCase $
  assertEqual
    "for (toValue (MyBoolArr [False,True]))"
    (A.Array $ V.fromList [A.Bool False, A.Bool True]) $
  toValue (MyBoolArr [False, True])

myIntegerArrToValueTest =
  TestCase $
  assertEqual
    "for (toValue (MyIntegerArr [1,2]))"
    (A.Array $ V.fromList [A.Number 1, A.Number 2]) $
  toValue (MyIntegerArr [1, 2])

myStringArrToValueTest =
  TestCase $
  assertEqual
    "for (toValue (MyStringArr [\"k1\",\"k2\"]))"
    (A.Array $ V.fromList [A.String "k1", A.String "k2"]) $
  toValue (MyStringArr ["k1", "k2"])

myNextValToValueTest =
  TestCase $
  assertEqual "for (toValue (MyNextval \"kek\"))" (A.String "kek") $
  toValue (MyNextval "kek")

myDateToValueTest =
  TestCase $
  assertEqual "for (toValue (MyDate \"2020-07-02\"))" (A.String "2020-07-02") $
  toValue (MyDate "2020-07-02")

myDateArrToValueTest =
  TestCase $
  assertEqual
    "for (toValue (MyDateArr [\"2020-07-02\",\"2020-07-02\"]))"
    (A.Array $ V.fromList [A.String "2020-07-02", A.String "2020-07-02"]) $
  toValue (MyDateArr ["2020-07-02", "2020-07-02"])

myUriToValueTest =
  TestCase $
  assertEqual
    "for (toValue (MyUri \"https://priziva.net/gor/\"))"
    (A.String "https://priziva.net/gor/") $
  toValue (MyUri "https://priziva.net/gor/")

myEmptyToValueTest =
  TestCase $ assertEqual "for (toValue MyEmpty)" A.Null $ toValue MyEmpty
