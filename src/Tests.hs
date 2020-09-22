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
import Tests.Request.Access.Check
import Tests.Request.Control
import Tests.Request.Handling
import Tests.Request.Method.Check
import Tests.Request.Params.Check
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
  requestAccessCheckTests <>
  requestHandlingTests <>
  requestMethodCheckTests <>
  requestParamsCheckTests <>
  showSqlTests <>
  sqlToValueTests <>
  databaseCreateTests <>
  databaseGetTests <> databaseTestTests <> databaseTests <> setupTests

runTest :: IO ()
runTest = void $ runTestTT testList
