module Tests.Value
    ( valueTests
    ) where

import           Data.Value

import           Data.Aeson
import qualified Data.Vector         as V

import Test.HUnit

valueTests =
    toTextArrTests
    <> toStrArrTests
    <> toTextTests
    <> toStrTests
    <> fromTests

toTextArrTests =
    [ TestLabel "array_toTextArrTest"   array_toTextArrTest
    , TestLabel "object_toTextArrTest"  object_toTextArrTest
    , TestLabel "null_toTextArrTest"    null_toTextArrTest
    , TestLabel "number_toTextArrTest"  number_toTextArrTest
    , TestLabel "string_toTextArrTest"  string_toTextArrTest
    , TestLabel "bool_toTextArrTest"    bool_toTextArrTest
    ]

array_toTextArrTest =
    TestCase $
    assertEqual "for (toTextArr (Array $ V.singleton $ String \"kek\"))"
    ["kek"]
    $ toTextArr (Array $ V.singleton $ String "kek")
object_toTextArrTest =
    TestCase $
    assertEqual "for (toTextArr (object [\"kek\" .= String \"kok\"]))"
    ["kek"]
    $ toTextArr (object ["kek" .= String "kok"])
null_toTextArrTest =
    TestCase $
    assertEqual "for (toTextArr Null)"
    []
    $ toTextArr Null
number_toTextArrTest =
    TestCase $
    assertEqual "for (toTextArr (Number 1))"
    ["1"]
    $ toTextArr (Number 1)
string_toTextArrTest =
    TestCase $
    assertEqual "for (toTextArr (String \"kek\"))"
    ["kek"]
    $ toTextArr (String "kek")
bool_toTextArrTest =
    TestCase $
    assertEqual "for (toTextArr (Bool True))"
    ["True"]
    $ toTextArr (Bool True)

toStrArrTests =
    [ TestLabel "array_toStrArrTest"   array_toStrArrTest
    , TestLabel "object_toStrArrTest"  object_toStrArrTest
    , TestLabel "null_toStrArrTest"    null_toStrArrTest
    , TestLabel "number_toStrArrTest"  number_toStrArrTest
    , TestLabel "string_toStrArrTest"  string_toStrArrTest
    , TestLabel "bool_toStrArrTest"    bool_toStrArrTest
    ]

array_toStrArrTest =
    TestCase $
    assertEqual "for (toStrArr (Array $ V.singleton $ String \"kek\"))"
    ["kek"]
    $ toStrArr (Array $ V.singleton $ String "kek")
object_toStrArrTest =
    TestCase $
    assertEqual "for (toStrArr (object [\"kek\" .= String \"kok\"]))"
    ["kek"]
    $ toStrArr (object ["kek" .= String "kok"])
null_toStrArrTest =
    TestCase $
    assertEqual "for (toStrArr Null)"
    []
    $ toStrArr Null
number_toStrArrTest =
    TestCase $
    assertEqual "for (toStrArr (Number 1))"
    ["1"]
    $ toStrArr (Number 1)
string_toStrArrTest =
    TestCase $
    assertEqual "for (toStrArr (String \"kek\"))"
    ["kek"]
    $ toStrArr (String "kek")
bool_toStrArrTest =
    TestCase $
    assertEqual "for (toStrArr (Bool True))"
    ["True"]
    $ toStrArr (Bool True)

toTextTests =
    [ TestLabel "number_toTextTest"  number_toTextTest
    , TestLabel "string_toTextTest"  string_toTextTest
    , TestLabel "bool_toTextTest"    bool_toTextTest
    , TestLabel "others_toTextTest"  others_toTextTest
    ]

number_toTextTest =
    TestCase $
    assertEqual "for (toText (Number 1))"
    "1"
    $ toText (Number 1)
string_toTextTest =
    TestCase $
    assertEqual "for (toText (String \"kek\"))"
    "kek"
    $ toText (String "kek")
bool_toTextTest =
    TestCase $
    assertEqual "for (toText (Bool False))"
    "False"
    $ toText (Bool False)
others_toTextTest =
    TestCase $
    assertEqual "for (toText Null)"
    ""
    $ toText Null

toStrTests =
    [ TestLabel "number_toStrTest"  number_toStrTest
    , TestLabel "string_toStrTest"  string_toStrTest
    , TestLabel "bool_toStrTest"    bool_toStrTest
    , TestLabel "others_toStrTest"  others_toStrTest
    ]

number_toStrTest =
    TestCase $
    assertEqual "for (toStr (Number 1))"
    "1"
    $ toStr (Number 1)
string_toStrTest =
    TestCase $
    assertEqual "for (toStr (String \"kek\"))"
    "kek"
    $ toStr (String "kek")
bool_toStrTest =
    TestCase $
    assertEqual "for (toStr (Bool False))"
    "False"
    $ toStr (Bool False)
others_toStrTest =
    TestCase $
    assertEqual "for (toStr Null)"
    ""
    $ toStr Null

fromTests =
    [ TestLabel "fromStrTest"               fromStrTest
    ]

fromStrTest =
    TestCase $
    assertEqual "for (fromStr \"kek\")"
    (String "kek")
    $ fromStr "kek"