module Tests
    ( runTest
    ) where

import Config

import Tests.Base

import Tests.Config

import Tests.Database
import Tests.Database.Create
import Tests.Database.Get
import Tests.Database.Test

import Tests.Empty
import Tests.Essence.Methods
import Tests.Essence.RelationsTree.Methods
import Tests.Essence.Parse
import Tests.Essence.Parse.Clause

import Tests.MyValue

import Tests.Required.Methods

import Tests.Request.Control
import Tests.Request.Access.Methods
import Tests.Request.Handling
import Tests.Request.Method.Methods
import Tests.Request.Params.Methods

import Tests.Value

import Tests.Setup
import Tests.SQL
import Tests.SQL.Actions
import Tests.SQL.ToValue

import qualified Data.HashMap.Strict as HM

import Test.HUnit

testList = TestList $ []
    <> baseTests
    <> configTests
    <> emptyTests
    <> essenceMethodsTests
    <> essenceRelationsTreeMethodsTests
    <> essenceParseTests
    <> essenceParseClauseTests
    <> myValueTests
    <> requiredMethodsTests
    <> requestControlTests
    <> requestAccessMethodsTests
    <> requestHandlingTests
    <> requestMethodMethodsTests
    <> requestParamsMethodsTests
    <> valueTests
    <> sqlTests
    <> sqlActionsTests
    <> sqlToValueTests
    <> databaseCreateTests
    <> databaseGetTests
    <> databaseTestTests
    <> databaseTests

runTest :: IO ()
runTest =
    if HM.null testConfig
        then runTestTT (TestList setupTests)
            >> print "Can't find Config.json" --- Need different look
        else runTestTT (TestList [TestList setupTests,testList])
            >> return ()