module Tests.MyValue
    ( myValueTests
    ) where

import Data.MyValue

import           Data.Aeson
import qualified Data.Vector           as V

import Test.HUnit

myValueTests =
    [ TestLabel "parseIntegersTest" parseIntegersTest
    , TestLabel "parseStringsTest"  parseStringsTest
    , TestLabel "parseBoolTest"     parseBoolTest
    ] <> chooseMyValueTests <> fromTests <> toTests

parseIntegersTest =
    TestCase $
    assertEqual "for (parseIntegers \"{1,2}\")"
    "[1,2]"
    $ parseIntegers "{1,2}"

parseStringsTest =
    TestCase $
    assertEqual "for (parseStrings \"[k1,k2]\")"
    "[\"k1\",\"k2\"]"
    $ parseStrings "[k1,k2]"

parseBoolTest =
    TestCase $
    assertEqual "for (parseBool \"false\")"
    "False"
    $ parseBool "false"

chooseMyValueTests =
    [ choose_integers_MyValueTest
    , choose_strings_MyValueTest
    , choose_bool_MyValueTest
    , choose_date_MyValueTest
    , choose_nextval_MyValueTest
    , choose_integer_MyValueTest
    , choose_string_MyValueTest
    , choose_empty_MyValueTest
    ]

choose_integers_MyValueTest =
    TestCase $
    assertEqual "for (chooseMyValue \"[1,2]\" \"[1,2]\")"
    (MyIntegers [1,2])
    $ chooseMyValue "[1,2]" "[1,2]"
choose_strings_MyValueTest =
    TestCase $
    assertEqual "for (chooseMyValue \"[K1,K2]\" \"[k1,k2]\")"
    (MyStrings ["k1","k2"])
    $ chooseMyValue "[K1,K2]" "[k1,k2]"
choose_bool_MyValueTest =
    TestCase $
    assertEqual "for (chooseMyValue \"FALSE\" \"false\")"
    (MyBool False)
    $ chooseMyValue "FALSE" "false"
choose_date_MyValueTest =
    TestCase $
    assertEqual "for (chooseMyValue \"2020-02-02\")"
    (MyDate "2020-02-02")
    $ chooseMyValue "2020-02-02" "2020-02-02"
choose_nextval_MyValueTest =
    TestCase $
    assertEqual "for (chooseMyValue \"NEXTVAL(PERSON_ID_SEQ)\" \"nextval(person_id_seq)\")"
    (MyNextval "nextval(person_id_seq)")
    $ chooseMyValue "NEXTVAL(PERSON_ID_SEQ)" "nextval(person_id_seq)"
choose_integer_MyValueTest =
    TestCase $
    assertEqual "for (chooseMyValue \"1\" \"1\")"
    (MyInteger 1)
    $ chooseMyValue "1" "1"
choose_string_MyValueTest =
    TestCase $
    assertEqual "for (chooseMyValue \"KEK\" \"kek\")"
    (MyString "kek")
    $ chooseMyValue "KEK" "kek"
choose_empty_MyValueTest =
    TestCase $
    assertEqual "for (chooseMyValue \"\" \"\")"
    MyEmpty
    $ chooseMyValue "" ""

---------------------------------------------------------------------------------
-----------------------------------From------------------------------------------
-----------------------------------Tests-----------------------------------------
---------------------------------------------------------------------------------

fromTests = fromBSTests <> fromValueTests <> fromStrTests

fromBSTests =
    [ TestLabel "fromBS_MyIntegers_Test"   fromBS_MyIntegers_Test
    , TestLabel "fromBS_MyStrings_Test"    fromBS_MyStrings_Test
    , TestLabel "fromBS_MyBool_Test"       fromBS_MyBool_Test
    , TestLabel "fromBS_MyDate_Test"       fromBS_MyDate_Test
    , TestLabel "fromBS_MyNextval_Test"    fromBS_MyNextval_Test
    , TestLabel "fromBS_MyInteger_Test"    fromBS_MyInteger_Test
    , TestLabel "fromBS_MyString_Test"     fromBS_MyString_Test
    , TestLabel "fromBS_MyEmpty_Test"      fromBS_MyEmpty_Test
    ]

fromBS_MyIntegers_Test =
    TestCase $
    assertEqual "for (fromBS \"[1,2]\")"
    (MyIntegers [1,2])
    $ fromBS "[1,2]"
fromBS_MyStrings_Test =
    TestCase $
    assertEqual "for (fromBS \"[\"k1\",\"k2\"]\")"
    (MyStrings ["k1","k2"])
    $ fromBS "[\"k1\",\"k2\"]"
fromBS_MyBool_Test =
    TestCase $
    assertEqual "for (fromBS \"false\")"
    (MyBool False)
    $ fromBS "false"
fromBS_MyDate_Test =
    TestCase $
    assertEqual "for (fromBS \"2020-07-01\")"
    (MyDate "2020-07-01")
    $ fromBS "2020-07-01"
fromBS_MyNextval_Test =
    TestCase $
    assertEqual "for (fromBS \"nextval(person_id_seq)\")"
    (MyNextval "nextval(person_id_seq)")
    $ fromBS "nextval(person_id_seq)"
fromBS_MyInteger_Test =
    TestCase $
    assertEqual "for (fromBS \"1\")"
    (MyInteger 1)
    $ fromBS "1"
fromBS_MyString_Test =
    TestCase $
    assertEqual "for (fromBS \"kek\")"
    (MyString "kek")
    $ fromBS "kek"
fromBS_MyEmpty_Test =
    TestCase $
    assertEqual "for (fromBS \"\")"
    MyEmpty
    $ fromBS ""

fromValueTests =
    [ TestLabel "fromValue_MyIntegers_Test"   fromValue_MyIntegers_Test
    , TestLabel "fromValue_MyStrings_Test"    fromValue_MyStrings_Test
    , TestLabel "fromValue_MyBool_Test"       fromValue_MyBool_Test
    , TestLabel "fromValue_MyDate_Test"       fromValue_MyDate_Test
    , TestLabel "fromValue_MyNextVal_Test"    fromValue_MyNextval_Test
    , TestLabel "fromValue_MyInteger_Test"    fromValue_MyInteger_Test
    , TestLabel "fromValue_MyString_Test"     fromValue_MyString_Test
    , TestLabel "fromValue_MyEmpty_Test"      fromValue_MyEmpty_Test
    ]

fromValue_MyIntegers_Test =
    TestCase $
    assertEqual "for (fromValue (Array $ V.fromList [Number 1,Number 2]))"
    (MyIntegers [1,2])
    $ fromValue (Array $ V.fromList [Number 1,Number 2])
fromValue_MyStrings_Test =
    TestCase $
    assertEqual "for (fromValue (Array $ V.fromList [String \"k1\",String \"k2\"]))"
    (MyStrings ["k1","k2"])
    $ fromValue (Array $ V.fromList [String "k1",String "k2"])
fromValue_MyBool_Test =
    TestCase $
    assertEqual "for (fromValue (Bool False))"
    (MyBool False)
    $ fromValue (Bool False)
fromValue_MyDate_Test =
    TestCase $
    assertEqual "for (fromValue (String \"2020-07-01\"))"
    (MyDate "2020-07-01")
    $ fromValue (String "2020-07-01")
fromValue_MyNextval_Test =
    TestCase $
    assertEqual "for (fromValue (String \"nextval(person_id_seq)\"))"
    (MyNextval "nextval(person_id_seq)")
    $ fromValue (String "nextval(person_id_seq)")
fromValue_MyInteger_Test =
    TestCase $
    assertEqual "for (fromValue (Number 1))"
    (MyInteger 1)
    $ fromValue (Number 1)
fromValue_MyString_Test =
    TestCase $
    assertEqual "for (fromValue (String \"kek\"))"
    (MyString "kek")
    $ fromValue (String "kek")
fromValue_MyEmpty_Test =
    TestCase $
    assertEqual "for (fromValue Null)"
    MyEmpty
    $ fromValue Null

fromStrTests =
    [ TestLabel "fromStr_MyIntegers_Test"   fromStr_MyIntegers_Test
    , TestLabel "fromStr_MyStrings_Test"    fromStr_MyStrings_Test
    , TestLabel "fromStr_MyBool_Test"       fromStr_MyBool_Test
    , TestLabel "fromStr_MyDate_Test"       fromStr_MyDate_Test
    , TestLabel "fromStr_MyNextval_Test"    fromStr_MyNextval_Test
    , TestLabel "fromStr_MyInteger_Test"    fromStr_MyInteger_Test
    , TestLabel "fromStr_MyString_Test"     fromStr_MyString_Test
    , TestLabel "fromStr_MyEmpty_Test"      fromStr_MyEmpty_Test
    ]

fromStr_MyIntegers_Test =
    TestCase $
    assertEqual "for (fromStr \"[1,2]\")"
    (MyIntegers [1,2])
    $ fromStr "[1,2]"
fromStr_MyStrings_Test =
    TestCase $
    assertEqual "for (fromStr \"[\"k1\",\"k2\"]\")"
    (MyStrings ["k1","k2"])
    $ fromStr "[\"k1\",\"k2\"]"
fromStr_MyBool_Test =
    TestCase $
    assertEqual "for (fromStr \"false\")"
    (MyBool False)
    $ fromStr "false"
fromStr_MyDate_Test =
    TestCase $
    assertEqual "for (fromStr \"2020-07-01\")"
    (MyDate "2020-07-01")
    $ fromStr "2020-07-01"
fromStr_MyNextval_Test =
    TestCase $
    assertEqual "for (fromStr \"nextval(person_id_seq)\")"
    (MyNextval "nextval(person_id_seq)")
    $ fromStr "nextval(person_id_seq)"
fromStr_MyInteger_Test =
    TestCase $
    assertEqual "for (fromStr \"1\")"
    (MyInteger 1)
    $ fromStr "1"
fromStr_MyString_Test =
    TestCase $
    assertEqual "for (fromStr \"kek\")"
    (MyString "kek")
    $ fromStr "kek"
fromStr_MyEmpty_Test =
    TestCase $
    assertEqual "for (fromStr \"\")"
    MyEmpty
    $ fromStr ""

---------------------------------------------------------------------------------
------------------------------------To-------------------------------------------
-----------------------------------Tests-----------------------------------------
---------------------------------------------------------------------------------

toTests = toStrTests <> toValueTests

toStrTests =
    [ TestLabel "myInteger_toStrTest"   myInteger_toStrTest
    , TestLabel "myString_toStrTest"    myString_toStrTest
    , TestLabel "myBool_toStrTest"      myBool_toStrTest
    , TestLabel "myIntegers_toStrTest"  myIntegers_toStrTest
    , TestLabel "myStrings_toStrTest"   myStrings_toStrTest
    , TestLabel "myNextVal_toStrTest"   myNextVal_toStrTest
    , TestLabel "myDate_toStrTest"      myDate_toStrTest
    , TestLabel "myEmpty_toStrTest"     myEmpty_toStrTest
    ]

myInteger_toStrTest =
    TestCase $
    assertEqual "for (toStr (MyInteger 1))"
    "1"
    $ toStr (MyInteger 1)
myString_toStrTest =
    TestCase $
    assertEqual "for (toStr (MyString \"kek\"))"
    "kek"
    $ toStr (MyString "kek")
myBool_toStrTest =
    TestCase $
    assertEqual "for (toStr (MyBool False))"
    "False"
    $ toStr (MyBool False)
myIntegers_toStrTest =
    TestCase $
    assertEqual "for (toStr (MyIntegers [1,2]))"
    "[1,2]"
    $ toStr (MyIntegers [1,2])
myStrings_toStrTest =
    TestCase $
    assertEqual "for (toStr (MyStrings [\"k1\",\"k2\"]))"
    "[k1,k2]"
    $ toStr (MyStrings ["k1","k2"])
myNextVal_toStrTest =
    TestCase $
    assertEqual "for (toStr (MyNextval \"kek\"))"
    "kek"
    $ toStr (MyNextval "kek")
myDate_toStrTest =
    TestCase $
    assertEqual "for (toStr (MyDate \"2020-07-02\"))"
    "2020-07-02"
    $ toStr (MyDate "2020-07-02")
myEmpty_toStrTest =
    TestCase $
    assertEqual "for (toStr MyEmpty)"
    ""
    $ toStr MyEmpty

toValueTests =
    [ TestLabel "myInteger_toValueTest"   myInteger_toValueTest
    , TestLabel "myString_toValueTest"    myString_toValueTest
    , TestLabel "myBool_toValueTest"      myBool_toValueTest
    , TestLabel "myIntegers_toValueTest"  myIntegers_toValueTest
    , TestLabel "myStrings_toValueTest"   myStrings_toValueTest
    , TestLabel "myNextVal_toValueTest"   myNextVal_toValueTest
    , TestLabel "myDate_toValueTest"      myDate_toValueTest
    , TestLabel "myEmpty_toValueTest"     myEmpty_toStrTest
    ]

myInteger_toValueTest =
    TestCase $
    assertEqual "for (toValue (MyInteger 1))"
    (Number 1)
    $ toValue (MyInteger 1)
myString_toValueTest =
    TestCase $
    assertEqual "for (toValue (MyString \"kek\"))"
    (String "kek")
    $ toValue (MyString "kek")
myBool_toValueTest =
    TestCase $
    assertEqual "for (toValue (MyBool False))"
    (Bool False)
    $ toValue (MyBool False)
myIntegers_toValueTest =
    TestCase $
    assertEqual "for (toValue (MyIntegers [1,2]))"
    (Array $ V.fromList [Number 1,Number 2])
    $ toValue (MyIntegers [1,2])
myStrings_toValueTest =
    TestCase $
    assertEqual "for (toValue (MyStrings [\"k1\",\"k2\"]))"
    (Array $ V.fromList [String "k1",String "k2"])
    $ toValue (MyStrings ["k1","k2"])
myNextVal_toValueTest =
    TestCase $
    assertEqual "for (toValue (MyNextval \"kek\"))"
    (String "kek")
    $ toValue (MyNextval "kek")
myDate_toValueTest =
    TestCase $
    assertEqual "for (toValue (MyDate \"2020-07-02\"))"
    (String "2020-07-02")
    $ toValue (MyDate "2020-07-02")
myEmpty_toValueTest =
    TestCase $
    assertEqual "for (toValue MyEmpty)"
    Null
    $ toValue MyEmpty