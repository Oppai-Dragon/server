module Tests.Base.Prelude
  ( basePreludeTests
  ) where

import Data.Base.Prelude

import Test.HUnit

basePreludeTests :: [Test]
basePreludeTests =
  [ TestLabel "deletePairTest" deletePairTest
  , TestLabel "replaceByTest" replaceByTest
  , TestLabel "ordToBoolTest" ordToBoolTest
  , TestLabel "lookup2Test" lookup2Test
  , TestLabel "tailCaseTest" tailCaseTest
  ]

deletePairTest, replaceByTest, ordToBoolTest, lookup2Test, tailCaseTest ::
     Test

deletePairTest =
  TestCase $
  assertEqual "for (deletePair (1,2) [(1,3),(2,3),(1,2)])" [(2, 3), (1, 2)] $
  deletePair ((1, 2) :: (Int, Int)) ([(1, 3), (2, 3), (1, 2)] :: [(Int, Int)])

replaceByTest =
  TestCase $
  assertEqual
    "for (replaceBy (==\"krut\") \"lox\" [\"misha\", \"krut\", \".\"])"
    ["misha", "lox", "."] $
  replaceBy (== "krut") "lox" (["misha", "krut", "."] :: [String])

ordToBoolTest = TestCase $ assertEqual "for (ordToBool GT)" False $ ordToBool GT

lookup2Test =
  TestCase $
  assertEqual
    "for (lookup2 \"person\" \"id\" [(\"person\",[(\"id\",1)])])"
    (Just 1) $
  lookup2 ("person" :: String) ("id" :: String) [("person", [("id", 1 :: Int)])]

tailCaseTest =
  TestCase $ assertEqual "for (tailCase [])" ([] :: String) $ tailCase []
