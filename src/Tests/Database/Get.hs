module Tests.Database.Get
  ( databaseGetTests
  ) where

import Config
import Data.Essence
import Data.MyValue
import Database.Get

import qualified Data.HashMap.Strict as HM

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Test.HUnit

databaseGetTests :: [Test]
databaseGetTests = [TestLabel "addOffsetLimitTest" addOffsetLimitTest]

addOffsetLimitTest :: Test
addOffsetLimitTest =
  TestCase $
  runReaderT (execStateT addOffsetLimit mempty) (Config HM.empty) >>=
  assertEqual
    "for (runReaderT (runStateT addOffsetLimit mempty) (Config HM.empty))"
    (EssenceList "" "" [("page", MyString "OFFSET 0 LIMIT 10")])
