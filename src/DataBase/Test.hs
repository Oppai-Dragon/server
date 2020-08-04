module Database.Test
    ( EssenceData
    , EssenceName
    , createEssenceTest
    , editEssenceTest
    , getEssenceTest
    , getOneTest
    , getArrayTest
    , deleteEssenceTest
    , getNeededFields
    , updateData
    , chooseNameForAdding
    , getRelatedFields
    , handleDraftCase
    , getRandomName
    , handleUniqueName
    ) where

import Config
import Data.Base
import Data.Essence
import Data.Essence.Methods
import Database.Create
import Database.Delete
import Database.Edit
import Database.Get
import Data.MyValue

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import qualified Data.Text           as T

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class          (lift)

import Test.HUnit

type EssenceList = [(String,MyValue)]
type EssenceData = [(String,EssenceList)]
type EssenceName = T.Text

-- | First create, then edit of get, and only then delete

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

getOneTest :: EssenceData -> ReaderT Config IO [Test]
getOneTest listData = do
    let essence = "author"
    let pare = case lookup2 essence "id" listData of
            Just x  -> ("id",x)
            Nothing -> ("id",MyEmpty)
    essenceValue <- dbGetOne (EssenceList essence "get" [pare])
    let test = TestCase $ testAction "dbGetOne" essence essenceValue
    return [test]

getArrayTest :: EssenceData -> ReaderT Config IO [Test]
getArrayTest listData = do
    let essence = "tag"
    let pare = case lookup2 essence "id" listData of
            Just (MyInteger id)  -> ("id",MyIntegers [id])
            Nothing              -> ("id",MyEmpty)
    essenceValue <- dbGetArray (EssenceList essence "get" [pare])
    let test = TestCase $ testAction "dbGetArray" essence essenceValue
    return [test]

deleteEssenceTest :: [String] -> EssenceData -> ReaderT Config IO [Test]
deleteEssenceTest []             _        = return []
deleteEssenceTest (essence:rest) listData = do
    let fields = fromJust (lookup essence listData)
    resultValue <- evalStateT dbDelete (EssenceList essence "delete" fields)
    let test = TestCase $ assertEqual ("for dbDelete for " <> essence) (object ["result" .= Number 1]) resultValue
    pure ((:) (TestLabel "dbDeleteTest" test)) <*> (deleteEssenceTest rest) listData

getNeededFields :: EssenceName -> Object -> EssenceList
getNeededFields "draft" obj =
    case parseMaybe (.: "draft1") obj of
        Just (Object fieldsObj) -> withoutEmpty . map
            (\(l,r) -> (T.unpack l,fromValue r)) $ HM.toList fieldsObj
        _                      -> []
getNeededFields essence obj =
    let parseFunc = parseFieldsFunc [essence <> "1","id"]
    in case parseMaybe parseFunc obj of
        Just value -> [("id",fromValue value)]
        _          -> []

updateData :: EssenceName -> Value -> StateT EssenceData (ReaderT Config IO) ()
updateData essence value = do
    currentData <- get
    let essenceObj = case value of {Object obj -> obj; _ -> HM.empty}
    let fieldsData = getNeededFields essence essenceObj
    put $ (T.unpack essence,fieldsData) : currentData

chooseNameForAdding :: String -> [String]
chooseNameForAdding name = case name of
    "author"   -> ["person"]
    "draft"    -> ["author","category","tag"]
    "news"     -> ["draft"]
    "comment"  -> ["person","news"]
    _          -> [name]

getRelatedFields :: String -> EssenceData -> EssenceList
getRelatedFields "news" currentData = concat
    [case lookup essence currentData of {
        Just xs -> map (\(l,r) -> if l == "id" then ("draft_id",r) else (l,r)) xs;
        Nothing -> []} | essence <- chooseNameForAdding "news"]
getRelatedFields name currentData = concat
    [case lookup essence currentData of {
        Just xs -> map (\(l,r) -> (essence <> "_" <> l,r)) xs;
        Nothing -> []} | essence <- chooseNameForAdding name]

handleDraftCase :: String -> EssenceList -> EssenceList
handleDraftCase name fields = case name of
    "draft" -> case lookup "tag_id" fields of
        Just (MyInteger id) -> ("tag_ids",MyIntegers [id])
            : Data.Base.deletePair ("tag_id",MyEmpty) fields
        _                   -> fields
    _       -> fields

getRandomName :: String -> IO String
getRandomName name = do
    randomNum <- getRandom
    let randomNumStr = show randomNum
    let result = name <> randomNumStr
    return result

handleUniqueName :: String -> EssenceList -> IO EssenceList
handleUniqueName name list = do
    uniqueName <- getRandomName name
    let myValue = MyString uniqueName
    let result = case name of
            "category" -> [("name",myValue)]
            "tag"      -> [("name",myValue)]
            _          -> list
    return result
