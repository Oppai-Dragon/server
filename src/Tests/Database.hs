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
import System.Random ( getStdRandom, randomR )

import Tests.Essence

import Test.HUnit

type EssenceName = T.Text

-- | First create, then edit of get, and only then delete
-- | Don't forget about funcions with access to database

getRandom :: IO Integer
getRandom = getStdRandom (randomR (1,100000000000))

isNull :: Value -> Bool
isNull result = case result of {Null -> True; _ -> False}

getNeededFields :: EssenceName -> Object -> [(String,MyValue)]
getNeededFields "draft" obj =
    case parseMaybe (.: "draft1") obj of
        Just (Object fieldsObj) -> map
            (\(l,r) -> case l of {"id" -> ("draft_id",fromValue r); _ -> (T.unpack l,fromValue r)})
            $ HM.toList fieldsObj
        _                      -> []
getNeededFields essence obj =
    let parseFunc = parseFieldsFunc [essence <> "1","id"]
    in case parseMaybe parseFunc obj of
        Just value -> [(T.unpack essence <> "_id",fromValue value)]
        _          -> []

updateData :: EssenceName -> Value -> StateT [(String,[(String,MyValue)])] (ReaderT Config IO) ()
updateData essence value = do
    currentData <- get
    let essenceObj = case value of {Object obj -> obj; _ -> HM.empty}
    let essenceStr = chooseNameForInserting $ T.unpack essence
    let fieldsData = getNeededFields essence essenceObj
    let fieldsCreateData = case fieldsData of
            [("tag_id",MyInteger id)] -> [("tag_ids",MyIntegers [id])]
            other                     -> other
    let createData = case lookup essenceStr currentData of
            Just essenceData -> (T.unpack essence,fieldsData) :
                insertPair (parseChoosingName essenceStr,essenceData <> fieldsCreateData) currentData
            Nothing          -> (essenceStr,fieldsCreateData) : currentData
    put createData

testAction :: String -> String -> Value -> Assertion
testAction funcName essenceName = assertBool
    ("fail " <> funcName <> " on " <> essenceName <> " list from Tests.Essence")
    . not . isNull

parseChoosingName :: String -> String
parseChoosingName = (<>) "for "

chooseNameForInserting :: String -> String
chooseNameForInserting name = case name of
    "category" -> "author"
    "tag"      -> "author"
    "news"     -> "person"
    _          -> name

chooseNameForAdding :: String -> String
chooseNameForAdding name = case name of
    "author"   -> "person"
    "draft"    -> "for draft"
    "news"     -> "draft"
    "comment"  -> "for comment"
    _          -> name

createEssenceTest :: [Essence List] -> StateT [(String,[(String,MyValue)])] (ReaderT Config IO) [Test]
createEssenceTest []                                                    = return []
createEssenceTest (essenceList@(EssenceList name "create" list') : rest) = do
    currentData <- get
    lift . lift $ print currentData
    let fields = case lookup (chooseNameForAdding name) currentData of
            Just x  -> x
            Nothing -> []
    randomNum <- lift . lift $ getRandom
    let randomValue = MyString $ name <> show randomNum
    let list = case name of
            "category" -> [("name",randomValue)]
            "tag"      -> [("name",randomValue)]
            _          -> list'
    essenceValue <- lift $ evalStateT dbCreate (EssenceList name "create" (list <> fields))
    lift . lift $ print essenceValue
    updateData (T.pack name) essenceValue
    let test = TestCase $ testAction "dbCreate" name essenceValue
    pure ((:) (TestLabel "dbCreateTest" test)) <*> (createEssenceTest rest)

editEssenceTest :: [String] -> StateT [(String,[(String,MyValue)])] (ReaderT Config IO) [Test]
editEssenceTest []             = return []
editEssenceTest (essence:rest) = do
    listData <- get
    let fields = fromJust (lookup essence listData)
    resultValue <- lift $ evalStateT dbEdit (EssenceList essence "edit" fields)
    lift . lift $ print resultValue
    let test = TestCase $ assertEqual ("for dbEdit for " <> essence) (object ["result" .= Number 1]) resultValue
    pure ((:) (TestLabel "dbEditTest" test)) <*> (editEssenceTest rest)

getEssenceTest :: [String] -> StateT [(String,[(String,MyValue)])] (ReaderT Config IO) [Test]
getEssenceTest []             = return []
getEssenceTest (essence:rest) = do
    listData <- get
    let fields = fromJust (lookup essence listData)
    essenceValue <- lift $ evalStateT dbGet (EssenceList essence "get" fields)
    lift . lift $ print essenceValue
    let test = TestCase $ testAction "dbGet" essence essenceValue
    pure ((:) (TestLabel "dbGetTest" test)) <*> (getEssenceTest rest)

deleteEssenceTest :: [String] -> StateT [(String,[(String,MyValue)])] (ReaderT Config IO) [Test]
deleteEssenceTest []             = return []
deleteEssenceTest (essence:rest) = do
    listData <- get
    let fields = fromJust (lookup essence listData)
    resultValue <- lift $ evalStateT dbGet (EssenceList essence "delete" fields)
    lift . lift $ print resultValue
    let test = TestCase $ assertEqual ("for dbDelete for " <> essence) (object ["result" .= Number 1]) resultValue
    pure ((:) (TestLabel "dbDeleteTest" test)) <*> (deleteEssenceTest rest)

databaseTests = [TestLabel "databaseTest" databaseTest]

databaseTest = TestList . unsafePerformIO $ runReaderT (
    evalStateT (
        createEssenceTest createEssenceList
        >> editEssenceTest essences
        >> getEssenceTest essences
        >> deleteEssenceTest (reverse essences)
        ) []
    ) testConfig