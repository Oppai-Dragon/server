module Tests.Database.Get
  ( databaseGetTests
  ) where

import Config
import Data.Essence
import Data.MyValue
import Database.Get

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Test.HUnit

databaseGetTests :: [Test]
databaseGetTests = [TestLabel "addOffsetLimitTest" addOffsetLimitTest]

addOffsetLimitTest :: Test
addOffsetLimitTest =
  TestCase $
  runReaderT (execStateT addOffsetLimit mempty) testHandle >>=
  assertEqual
    "for (runReaderT (runStateT addOffsetLimit mempty) (Config HM.empty))"
    mempty {elList = [("page", MyString "OFFSET 0 LIMIT 10")]}
