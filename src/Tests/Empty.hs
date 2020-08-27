module Tests.Empty
  ( emptyTests
  ) where

import Data.Empty

import Test.HUnit

emptyTests :: [Test]
emptyTests =
  [ TestLabel "parseStringValueTest" parseStringValueTest
  , TestLabel "parseStringsValueTest" parseStringsValueTest
  , TestLabel "parseIntegerValueTest" parseIntegerValueTest
  , TestLabel "parseIntegersValueTest" parseIntegersValueTest
  , TestLabel "parseBoolValueTest" parseBoolValueTest
  ]

parseStringValueTest, parseStringsValueTest, parseIntegerValueTest, parseIntegersValueTest, parseBoolValueTest ::
     Test
parseStringValueTest =
  TestCase $
  assertEqual "for (parseValue \"misha\")" "'misha'" $
  parseValue ("misha" :: String)

parseStringsValueTest =
  TestCase $
  assertEqual "for (parseValue [\"misha\",\"lox\"])" "ARRAY['misha','lox']" $
  parseValue (["misha", "lox"] :: [String])

parseIntegerValueTest =
  TestCase $ assertEqual "for (parseValue 1)" "1" $ parseValue (1 :: Integer)

parseIntegersValueTest =
  TestCase $
  assertEqual "for (parseValue [1,2,3])" "ARRAY[1,2,3]" $
  parseValue ([1, 2, 3] :: [Integer])

parseBoolValueTest =
  TestCase $ assertEqual "for (parseValue False)" "False" $ parseValue False
