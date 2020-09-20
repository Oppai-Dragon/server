module Tests
  ( runTest
  ) where

import Config
import Log

import Tests.Base
import Tests.Config
import Tests.Database
import Tests.Database.Create
import Tests.Database.Get
import Tests.Database.Test
import Tests.Empty
import Tests.Essence.GetFields
import Tests.Essence.Methods
import Tests.Essence.Parse.Clause
import Tests.Essence.RelationsTree.Methods
import Tests.MyValue
import Tests.Request
import Tests.Request.Access.IsRight
import Tests.Request.Control
import Tests.Request.Handling
import Tests.Request.Method.IsRight
import Tests.Request.Params.Methods
import Tests.Required.Methods
import Tests.SQL.ShowSql
import Tests.SQL.ToValue
import Tests.Setup

import Control.Monad
import qualified Data.HashMap.Strict as HM

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
  essenceParseClauseTests <>
  myValueTests <>
  requiredMethodsTests <>
  requestTests <>
  requestControlTests <>
  requestAccessIsRightTests <>
  requestHandlingTests <>
  requestMethodIsRightTests <>
  requestParamsMethodsTests <>
  showSqlTests <>
  sqlToValueTests <>
  databaseCreateTests <> databaseGetTests <> databaseTestTests <> databaseTests

runTest :: IO ()
runTest = do
  (Config.Handle (Config config) _ _ _) <- Config.new
  if HM.null config
    then runTestTT (TestList setupTests) >>
         infoIO "Can't find Config.json" --- Need different look
    else void $ runTestTT (TestList [TestList setupTests, testList])
