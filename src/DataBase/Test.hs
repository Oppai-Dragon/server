module Database.Test
    ( EssenceData
    , EssenceName
    , goodResultValue
    , getDefaultValues
    , buildEssenceValue
    , getTest
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
import Data.Essence hiding (name)
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
import qualified Data.List           as L
import qualified Data.Text           as T

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class          (lift)

import Tests.Essence

import Test.HUnit

type EssenceList = [(String,MyValue)]
type EssenceData = [(String,EssenceList)]
type EssenceName = T.Text

-- | First create, then edit of get, and only then delete

testAction :: String -> String -> Value -> Value -> Test
testAction funcName essenceName = (TestCase .) . assertEqual ("fail " <> funcName <> " on " <> essenceName)

goodResultValue :: Value
goodResultValue = object ["result" .= Number 1]

getDefaultValues :: [String] -> [(T.Text,Value)]
getDefaultValues []           = []
getDefaultValues (field:rest) =
    case lookup (T.pack field) defaultList of
        Just value -> (T.pack field,value) : getDefaultValues rest
        Nothing    -> (T.pack field,Null) : getDefaultValues rest

buildEssenceValue :: Essence List -> StateT EssenceData (ReaderT Config IO) Value
buildEssenceValue (EssenceList _    "edit"   _)  = pure goodResultValue
buildEssenceValue (EssenceList _    "delete" _)  = pure goodResultValue
buildEssenceValue (EssenceList name action   _)  = do
    currentData <- get
    let list = handleDraftCase name $ getRelatedFields name currentData
    fromStateT $ print list
    api <- fromStateT setApi
    config <- lift ask
    let (EssenceDB essence action' hashMap) = getEssenceDB (T.pack name) (T.pack action) config api
    let fields = HM.keys hashMap L.\\ (fst . unzip) list
    let hmList = map (\(l,r) -> (T.pack l, toValue r)) list
    let fieldsValue = Object . HM.fromList $ hmList <> getDefaultValues fields
    let essenceValue = object [(<>) essence "1" .= fieldsValue]
    return essenceValue
buildEssenceValue _                             = pure $ object []

getTest :: Essence List -> String -> Value -> StateT EssenceData (ReaderT Config IO) Test
getTest essenceList funcName got = do
    expected <- buildEssenceValue essenceList
    let test = testAction funcName name expected got
    pure test

createEssenceTest :: [Essence List] -> StateT EssenceData (ReaderT Config IO) [Test]
createEssenceTest []                                                    = return []
createEssenceTest ((EssenceList name "create" list') : rest) = do
    currentData <- get
    fromStateT $ print "CURRENT DATA "
    fromStateT $ print currentData
    let fields = handleDraftCase name $ getRelatedFields name currentData
    list <- fromStateT $ handleUniqueName name list'
    let essenceList = EssenceList name "create" (list <> fields)
    fromStateT $ print essenceList
    essenceValue <- lift $ evalStateT dbCreate essenceList
    fromStateT . print $ "CREATED " <> name
    updateData (T.pack name) essenceValue
    let funcName = "dbCreate"
    test <- getTest essenceList funcName essenceValue
    let label = funcName <> "_" <> name <> "_Test"
    pure ((:) (TestLabel label test)) <*> (createEssenceTest rest)

editEssenceTest :: [String] -> EssenceData -> ReaderT Config IO [Test]
editEssenceTest []             _        = return []
editEssenceTest (essence:rest) listData = do
    let fields = fromJust (lookup essence listData)
    resultValue <- evalStateT dbEdit (EssenceList essence "edit" fields)
    let funcName = "dbEdit"
    test <- evalStateT (getTest (EssenceList essence "edit" []) funcName resultValue) listData
    let label = funcName <> "_" <> essence <> "_Test"
    pure ((:) (TestLabel label test)) <*> (editEssenceTest rest) listData

getEssenceTest :: [String] -> EssenceData -> ReaderT Config IO [Test]
getEssenceTest []             _        = return []
getEssenceTest (essence:rest) listData = do
    let fields = fromJust (lookup essence listData)
    essenceValue <- evalStateT dbGet (EssenceList essence "get" fields)
    let funcName = "dbGet"
    test <- evalStateT (getTest (EssenceList essence "get" fields) funcName essenceValue) listData
    let label = funcName <> "_" <> essence <> "_Test"
    pure ((:) (TestLabel label test)) <*> (getEssenceTest rest) listData

getOneTest :: EssenceData -> ReaderT Config IO [Test]
getOneTest listData = do
    let essence = "author"
    let pare = case lookup2 essence "id" listData of
            Just x  -> ("id",x)
            Nothing -> ("id",MyEmpty)
    let essenceList = EssenceList essence "get" [pare]
    essenceValue <- dbGetOne essenceList
    let funcName = "dbGetOne"
    test <- evalStateT (getTest essenceList funcName essenceValue) listData
    let label = funcName <> "_" <> essence <> "_Test"
    return [TestLabel label test]

getArrayTest :: EssenceData -> ReaderT Config IO [Test]
getArrayTest listData = do
    let essence = "tag"
    let pare = case lookup2 essence "id" listData of
            Just (MyInteger id)  -> ("id",MyIntegers [id])
            Nothing              -> ("id",MyEmpty)
    let essenceList = EssenceList essence "get" [pare]
    essenceValue <- dbGetArray essenceList
    let funcName = "dbGetArray"
    test <- evalStateT (getTest essenceList "dbGetArray" essenceValue) listData
    let label = funcName <> "_" <> essence <> "_Test"
    return [TestLabel label test]

deleteEssenceTest :: [String] -> EssenceData -> ReaderT Config IO [Test]
deleteEssenceTest []             _        = return []
deleteEssenceTest (essence:rest) listData = do
    let fields = fromJust (lookup essence listData)
    resultValue <- evalStateT dbDelete (EssenceList essence "delete" fields)
    let funcName = "dbDelete"
    test <- evalStateT (getTest (EssenceList essence "delete" []) funcName resultValue) listData
    let label = funcName <> "_" <> essence <> "_Test"
    pure ((:) (TestLabel label test)) <*> (deleteEssenceTest rest) listData

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
    modify $ (:) (T.unpack essence,fieldsData)

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
