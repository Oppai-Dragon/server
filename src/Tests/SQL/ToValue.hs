module Tests.SQL.ToValue
    ( sqlToValueTests
    ) where

import Config
import Data.Essence
import Data.MyValue
import Data.SQL.ToValue

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Database.HDBC

import Test.HUnit

sqlToValueTests =
    [ TestLabel "sqlToValueTest"            sqlToValueTest
    , TestLabel "fromZipTest"               fromZipTest
    , TestLabel "sqlValuesToJsonValueTest"  sqlValuesToJsonValueTest
    , TestLabel "sqlValuesArrToObjTest"     sqlValuesArrToObjTest
    , TestLabel "sqlValuesArrToValueTest"   sqlValuesArrToValueTest
    ]

sqlToValueTest =
    TestCase $
    assertEqual "for (sqlToValue (SqlInteger 1))"
    (Number 1)
    $ sqlToValue (SqlInteger 1)

fromZipTest =
    TestCase $
    assertEqual "for (fromZip [\"person_id\"] [Number 1])"
    (HM.singleton "person_id" (Number 1))
    $ fromZip ["person_id"] [Number 1]

sqlValuesToJsonValueTest =
    TestCase $
    assertEqual
    "for (sqlValuesToJsonValue (EssenceList \"author\" \"create\" [(\"person_id\",\"1\")]) [SqlInteger 1] testConfig)"
    (object ["person_id" .= Number 1])
    $ sqlValuesToJsonValue (EssenceList "author" "create" [("person_id",MyInteger 1)]) [SqlInteger 1] testConfig

sqlValuesArrToObjTest =
    TestCase $
    assertEqual
    "for (sqlValuesArrToObj 1 (EssenceList \"author\" \"create\" [(\"person_id\",\"1\")]) [[SqlInteger 1]] testConfig)"
    (HM.singleton "author1" (object ["person_id" .= Number 1]))
    $ sqlValuesArrToObj 1 (EssenceList "author" "create" [("person_id",MyInteger 1)]) [[SqlInteger 1]] testConfig

sqlValuesArrToValueTest =
    TestCase $
    assertEqual
    "for (sqlValuesArrToValue (EssenceList \"author\" \"create\" [(\"person_id\",\"1\")]) [[SqlInteger 1]] testConfig)"
    (object ["author1" .= object ["person_id" .= Number 1]])
    $ sqlValuesArrToValue (EssenceList "author" "create" [("person_id",MyInteger 1)]) [[SqlInteger 1]] testConfig
