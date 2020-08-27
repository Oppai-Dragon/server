module Tests.Base
  ( baseTests
  ) where

import Data.Base
import Data.Monoid

import Test.HUnit

baseTests :: [Test]
baseTests =
  [ TestLabel "parsePathTest" parsePathTest
  , TestLabel "ifElseThenTest" ifElseThenTest
  , TestLabel "reverseMapTest" reverseMapTest
  , TestLabel "map2VarTest" map2VarTest
  , TestLabel "deletePairTest" deletePairTest
  , TestLabel "insertPairTest" insertPairTest
  , TestLabel "mapToListOfPairTest" mapToListOfPairTest
  , TestLabel "findTextTest" findTextTest
  , TestLabel "scientificToIntegerTest" scientificToIntegerTest
  , TestLabel "replaceByTest" replaceByTest
  , TestLabel "ordToBoolTest" ordToBoolTest
  , TestLabel "lookup2Test" lookup2Test
  , TestLabel "tailCaseTest" tailCaseTest
  ]

parsePathTest, ifElseThenTest, reverseMapTest, map2VarTest, deletePairTest, insertPairTest, mapToListOfPairTest, findTextTest, scientificToIntegerTest, replaceByTest, ordToBoolTest, lookup2Test, tailCaseTest ::
     Test
parsePathTest =
  TestCase $
  assertEqual "for (parsePath \"C:Repo\\server\\src\")" "C:Repo\\server" $
  parsePath "C:Repo\\server\\src"

ifElseThenTest =
  TestCase $
  assertEqual "for (ifElseThen [True,False] [1,2,3])" 2 $
  ifElseThen [True, False] ([Sum 1, Sum 2, Sum 3] :: [Sum Int])

reverseMapTest =
  TestCase $
  assertEqual "for (reverseMap 1 [(*2),(+2)])" [2, 3] $
  reverseMap (1 :: Integer) [(* 2), (+ 2)]

map2VarTest =
  TestCase $
  assertEqual "for (map2Var const [1,2,3,4,5,6])" [1, 3, 6, 10, 15, 21] $
  map2Var (+) ([1, 2, 3, 4, 5, 6] :: [Int])

deletePairTest =
  TestCase $
  assertEqual "for (deletePair (1,2) [(1,3),(2,3),(1,2)])" [(2, 3), (1, 2)] $
  deletePair ((1, 2) :: (Int, Int)) ([(1, 3), (2, 3), (1, 2)] :: [(Int, Int)])

insertPairTest =
  TestCase $
  assertEqual
    "for (insertPair (1,2) [(1,3),(2,3),(1,4)])"
    [(1, 2), (2, 3), (1, 4)] $
  insertPair ((1, 2) :: (Int, Int)) ([(1, 3), (2, 3), (1, 4)] :: [(Int, Int)])

mapToListOfPairTest =
  TestCase $
  assertEqual
    "for (mapToListOfPair (+1) [(1,3),(2,3),(1,4)])"
    [(2, 4), (3, 4), (2, 5)] $
  mapToListOfPair (+ 1) ([(1, 3), (2, 3), (1, 4)] :: [(Int, Int)])

findTextTest =
  TestCase $
  assertEqual "for (findText \"k1\" [\"k2\",\"k1\"])" (Just "k1") $
  findText "k1" ["k2", "k1"]

scientificToIntegerTest =
  TestCase $
  assertEqual "for (scientificToInteger 1234567890)" 1234567890 $
  scientificToInteger 1234567890

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
