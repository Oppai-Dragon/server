module Tests.Database.Get
    ( databaseGetTests
    ) where

import Config
import Database.Get
import Data.Essence
import Data.MyValue

import qualified Data.HashMap.Strict as HM

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict

import Test.HUnit

databaseGetTests =
    [ TestLabel "addOffsetLimitTest"    addOffsetLimitTest
    ]

addOffsetLimitTest =
    TestCase $
    runReaderT (execStateT addOffsetLimit mempty) (Config HM.empty) >>=
    assertEqual
    "for (runReaderT (runStateT addOffsetLimit mempty) (Config HM.empty))"
    (EssenceList "" "" [("page",MyString "OFFSET 0 LIMIT 10")])
