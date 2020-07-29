module Tests.Empty
    ( emptyTests
    ) where

import Data.Empty
import Data.MyValue

import Test.HUnit

emptyTests =
    [ TestLabel "parse_string_ValueTest"    parse_string_ValueTest
    , TestLabel "parse_strings_ValueTest"   parse_strings_ValueTest
    , TestLabel "parse_integer_ValueTest"   parse_integer_ValueTest
    , TestLabel "parse_integers_ValueTest"  parse_integers_ValueTest
    , TestLabel "parse_bool_ValueTest"      parse_bool_ValueTest
    ]

parse_string_ValueTest =
    TestCase $
    assertEqual "for (parseValue \"misha\")"
    "'misha'"
    $ parseValue "misha"
parse_strings_ValueTest =
    TestCase $
    assertEqual "for (parseValue [\"misha\",\"lox\"])"
    "ARRAY['misha','lox']"
    $ parseValue ["misha","lox"]
parse_integer_ValueTest =
    TestCase $
    assertEqual "for (parseValue 1)"
    "1"
    $ parseValue 1
parse_integers_ValueTest =
    TestCase $
    assertEqual "for (parseValue [1,2,3])"
    "ARRAY[1,2,3]"
    $ parseValue [1,2,3]
parse_bool_ValueTest =
    TestCase $
    assertEqual "for (parseValue False)"
    "False"
    $ parseValue False