{-# LANGUAGE LambdaCase #-}
module Tests.Database
    ( databaseTests
    ) where

import Config
import Database.Test

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import System.IO.Unsafe (unsafePerformIO)

import Tests.Essence

import Test.HUnit

databaseTests :: [Test]
databaseTests = unsafePerformIO $ runReaderT
    (( do
        (testListCreate,essenceData) <- runStateT (createEssenceTest createEssenceList) []
        testListEdit <- editEssenceTest essences essenceData
        testListGet <- getEssenceTest essences essenceData
        testListGetOne <- getOneTest essenceData
        testListGetArray <- getArrayTest essenceData
        testListDelete <- deleteEssenceTest (reverse essences) essenceData
        let tests = testListCreate
                <> testListEdit
                <> testListGet
                <> testListGetOne
                <> testListGetArray
                <> testListDelete
        return tests
    )) testConfig