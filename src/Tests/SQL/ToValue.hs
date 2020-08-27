module Tests.SQL.ToValue
  ( sqlToValueTests
  ) where

import Config
import Data.Essence
import Data.MyValue
import Data.SQL.ToValue

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Database.HDBC as HDBC

import Test.HUnit

sqlToValueTests :: [Test]
sqlToValueTests =
  [ TestLabel "sqlToValueTest" sqlToValueTest
  , TestLabel "fromZipTest" fromZipTest
  , TestLabel "sqlValuesToJsonValueTest" sqlValuesToJsonValueTest
  , TestLabel "sqlValuesArrToObjTest" sqlValuesArrToObjTest
  , TestLabel "sqlValuesArrToValueTest" sqlValuesArrToValueTest
  ]

sqlToValueTest, fromZipTest, sqlValuesToJsonValueTest, sqlValuesArrToObjTest, sqlValuesArrToValueTest ::
     Test
sqlToValueTest =
  TestCase $
  assertEqual "for (sqlToValue (SqlInteger 1))" (A.Number 1) $
  sqlToValue (HDBC.SqlInteger 1)

fromZipTest =
  TestCase $
  assertEqual
    "for (fromZip [\"person_id\"] [Number 1])"
    (HM.singleton "person_id" (A.Number 1)) $
  fromZip ["person_id"] [A.Number 1]

sqlValuesToJsonValueTest =
  TestCase $
  assertEqual
    "for (sqlValuesToJsonValue (EssenceList \"author\" \"create\" [(\"person_id\",\"1\")]) [SqlInteger 1] testConfig)"
    (A.object ["person_id" A..= A.Number 1]) $
  sqlValuesToJsonValue
    (EssenceList "author" "create" [("person_id", MyInteger 1)])
    [HDBC.SqlInteger 1]
    testConfig

sqlValuesArrToObjTest =
  TestCase $
  assertEqual
    "for (sqlValuesArrToObj 1 (EssenceList \"author\" \"create\" [(\"person_id\",\"1\")]) [[SqlInteger 1]] testConfig)"
    (HM.singleton "author1" (A.object ["person_id" A..= A.Number 1])) $
  sqlValuesArrToObj
    1
    (EssenceList "author" "create" [("person_id", MyInteger 1)])
    [[HDBC.SqlInteger 1]]
    testConfig

sqlValuesArrToValueTest =
  TestCase $
  assertEqual
    "for (sqlValuesArrToValue (EssenceList \"author\" \"create\" [(\"person_id\",\"1\")]) [[SqlInteger 1]] testConfig)"
    (A.object ["author1" A..= A.object ["person_id" A..= A.Number 1]]) $
  sqlValuesArrToValue
    (EssenceList "author" "create" [("person_id", MyInteger 1)])
    [[HDBC.SqlInteger 1]]
    testConfig
