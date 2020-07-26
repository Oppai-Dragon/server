module Tests
    ( runTest
    ) where

import Tests.Required.Methods
import Tests.SQL.ToValue
import Tests.Base
import Tests.MyValue
import Tests.Value

import Test.HUnit

runTest :: IO Counts
runTest = runTestTT . TestList
    $ []
    <> requiredMethodsTests
    <> sqlToValueTests
    <> baseTests
    <> myValueTests
    <> valueTests