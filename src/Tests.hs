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
import Tests.Essence.GetFields
import Tests.Essence.Methods
import Tests.Essence.Parse
import Tests.Essence.Parse.Clause
import Tests.Essence.RelationsTree.Methods
import Tests.MyValue
import Tests.Request
import Tests.Request.Access.Methods
import Tests.Request.Control
import Tests.Request.Handling
import Tests.Request.Method.Methods
import Tests.Request.Params.Methods
import Tests.Required.Methods
import Tests.SQL.ShowSql
import Tests.SQL.ToValue
import Tests.Setup
import Tests.Value

import Control.Monad
import qualified Data.HashMap.Strict as HM
import Debug.Trace

import Test.HUnit

testList :: Test
testList =
  TestList $
  baseTests <>
  configTests <>
  emptyTests <>
  essenceGetFieldsTests <>
  essenceMethodsTests <>
  essenceRelationsTreeMethodsTests <>
  essenceParseTests <>
  essenceParseClauseTests <>
  myValueTests <>
  requiredMethodsTests <>
  requestTests <>
  requestControlTests <>
  requestAccessMethodsTests <>
  requestHandlingTests <>
  requestMethodMethodsTests <>
  requestParamsMethodsTests <>
  valueTests <>
  showSqlTests <>
  sqlToValueTests <>
  databaseCreateTests <> databaseGetTests <> databaseTestTests <> databaseTests

runTest :: IO ()
runTest = do
  (Config.Handle (Config config) _ _ _) <- Config.new
  if HM.null config
    then runTestTT (TestList setupTests) >>
         traceIO "Can't find Config.json" --- Need different look
    else void $ runTestTT (TestList [TestList setupTests, testList])
