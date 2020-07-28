module Tests
    ( runTest
    ) where

import Tests.Base

import Tests.Config

import Tests.Essence.Methods
import Tests.Essence.RelationsTree.Methods
import Tests.Essence.Parse

import Tests.MyValue

import Tests.Required.Methods

import Tests.Request.Control
import Tests.Request.Access.Methods
import Tests.Request.Method.Methods
import Tests.Request.Params.Methods

import Tests.Value

import Tests.Setup
import Tests.SQL
import Tests.SQL.ToValue

import Test.HUnit

runTest :: IO Counts
runTest = runTestTT . TestList
    $ []
    <> baseTests
    <> configTests
    <> essenceMethodsTests
    <> essenceRelationsTreeMethodsTests
    <> essenceParseTests
    <> myValueTests
    <> requiredMethodsTests
    <> requestControlTests
    <> requestAccessMethodsTests
    <> requestMethodMethodsTests
    <> requestParamsMethodsTests
    <> valueTests
    <> setupTests
    <> sqlTests
    <> sqlToValueTests