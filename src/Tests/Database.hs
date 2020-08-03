{-# LANGUAGE LambdaCase #-}
module Tests.Database
    ( databaseTests
    ) where

import Config
import Data.Base
import Database.Create
import Database.Delete
import Database.Edit
import Database.Get
import Database.Test
import Data.Essence
import Data.MyValue

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import qualified Data.Text           as T

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Class          (lift)

import System.IO.Unsafe (unsafePerformIO)

import Tests.Essence

import Test.HUnit

-- | First create, then edit of get, and only then delete
-- | Don't forget about funcions with access to database

testAction :: String -> String -> Value -> Assertion
testAction funcName essenceName (Object obj) = assertBool
    ("fail " <> funcName <> " on " <> essenceName)
    . not $ HM.null obj

createEssenceTest :: [Essence List] -> StateT EssenceData (ReaderT Config IO) [Test]
createEssenceTest []                                                    = return []
createEssenceTest (essenceList@(EssenceList name "create" list') : rest) = do
    currentData <- get
    let fields = handleDraftCase name $ getRelatedFields name currentData
    list <- fromStateT $ handleUniqueName name list'
    essenceValue <- lift $ evalStateT dbCreate (EssenceList name "create" (list <> fields))
    updateData (T.pack name) essenceValue
    let test = TestCase $ testAction "dbCreate" name essenceValue
    pure ((:) (TestLabel "dbCreateTest" test)) <*> (createEssenceTest rest)

editEssenceTest :: [String] -> EssenceData -> ReaderT Config IO [Test]
editEssenceTest []             _        = return []
editEssenceTest (essence:rest) listData = do
    let fields = fromJust (lookup essence listData)
    resultValue <- evalStateT dbEdit (EssenceList essence "edit" fields)
    let test = TestCase $ assertEqual ("for dbEdit for " <> essence) (object ["result" .= Number 1]) resultValue
    pure ((:) (TestLabel "dbEditTest" test)) <*> (editEssenceTest rest) listData

getEssenceTest :: [String] -> EssenceData -> ReaderT Config IO [Test]
getEssenceTest []             _        = return []
getEssenceTest (essence:rest) listData = do
    let fields = fromJust (lookup essence listData)
    essenceValue <- evalStateT dbGet (EssenceList essence "get" fields)
    let test = TestCase $ testAction "dbGet" essence essenceValue
    pure ((:) (TestLabel "dbGetTest" test)) <*> (getEssenceTest rest) listData

deleteEssenceTest :: [String] -> EssenceData -> ReaderT Config IO [Test]
deleteEssenceTest []             _        = return []
deleteEssenceTest (essence:rest) listData = do
    let fields = fromJust (lookup essence listData)
    resultValue <- evalStateT dbDelete (EssenceList essence "delete" fields)
    let test = TestCase $ assertEqual ("for dbDelete for " <> essence) (object ["result" .= Number 1]) resultValue
    pure ((:) (TestLabel "dbDeleteTest" test)) <*> (deleteEssenceTest rest) listData

databaseTests = unsafePerformIO $ runReaderT
    (( do
        (testListCreate,essenceData) <- runStateT (createEssenceTest createEssenceList) []
        testListEdit <- editEssenceTest essences essenceData
        testListGet <- getEssenceTest essences essenceData
        testListDelete <- deleteEssenceTest (reverse essences) essenceData
        let tests = testListCreate <> testListEdit <> testListGet <> testListDelete
        return tests
    )) testConfig