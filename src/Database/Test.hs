module Database.Test
  ( EssenceData
  , EssenceName
  , goodResultValue
  , getDefaultValues
  , buildEssenceValue
  , updateHmList
  , updateEssenceList
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
  ) where

import Config
import Data.Base
import Data.Essence
import Data.Essence.Methods
import Data.MyValue
import Database.Create
import Database.Delete
import Database.Edit
import Database.Get

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Maybe as Maybe
import qualified Data.Text as T

--import Debug.Trace
import Tests.Essence

import qualified Test.HUnit as Test

type EssenceList = [(String, MyValue)]

type EssenceData = [(String, EssenceList)]

type EssenceName = T.Text

-- | First create, then edit of get, and only then delete
testAction :: (Eq a, Show a) => String -> String -> a -> a -> Test.Test
testAction funcName essenceName =
  (Test.TestCase .) .
  Test.assertEqual ("fail " <> funcName <> " on " <> essenceName)

goodResultValue :: A.Value
goodResultValue = A.object ["result" A..= A.Number 1]

getDefaultValues :: [String] -> [(T.Text, A.Value)]
getDefaultValues [] = []
getDefaultValues (field:rest) =
  case lookup (T.pack field) defaultList of
    Just value -> (T.pack field, value) : getDefaultValues rest
    Nothing -> (T.pack field, A.Null) : getDefaultValues rest

buildEssenceValue :: Essence List -> A.Value
buildEssenceValue (EssenceList name action _) =
  case action of
    "edit" -> goodResultValue
    "delete" -> goodResultValue
    _ -> A.object [T.pack (name <> "1") A..= A.Null]

updateHmList :: T.Text -> [(T.Text, A.Value)] -> [(T.Text, A.Value)]
updateHmList _ [] = []
updateHmList essence ((field, value):rest) =
  if T.take (T.length essence) field == essence
    then ("id", value) : rest
    else (field, value) : updateHmList essence rest

updateEssenceList :: String -> [(String, MyValue)] -> [(String, MyValue)]
updateEssenceList _ [] = []
updateEssenceList essence ((field, value):rest) =
  if take (length essence) field == essence
    then ("id", value) : rest
    else (field, value) : updateEssenceList essence rest

getTest :: Essence List -> String -> A.Value -> Test.Test
getTest essenceList@(EssenceList name action _) funcName got@(A.Object gotObj) =
  let expected@(A.Object expectedObj) = buildEssenceValue essenceList
   in case action of
        "edit" -> testAction funcName name expected got
        "delete" -> testAction funcName name expected got
        _ -> testAction funcName name (HM.keys expectedObj) (HM.keys gotObj)
getTest _ _ _ = Test.TestCase $ Test.assertBool "getTest take only " False

createEssenceTest :: [Essence List] -> S EssenceData UnderApp [Test.Test]
createEssenceTest ((EssenceList name "create" list):rest) = do
  currentData <- getSApp
  --liftUnderApp . liftIO $ traceIO "CURRENT DATA "
  --liftUnderApp . liftIO . traceIO $ show currentData
  let fields = handleDraftCase name $ getRelatedFields name currentData
  let essenceList = EssenceList name "create" (list <> fields)
  --liftUnderApp . liftIO . traceIO $ show essenceList
  essenceValue <- liftUnderApp $ evalSApp dbCreate essenceList
  --liftUnderApp . liftIO . traceIO $ "CREATED " <> name
  updateData (T.pack name) essenceValue
  let funcName = "createEssenceTest"
  let test = getTest essenceList funcName essenceValue
  let label = funcName <> "_" <> name <> "_Test"
  (Test.TestLabel label test :) <$> createEssenceTest rest
createEssenceTest _ = return []

editEssenceTest :: [String] -> EssenceData -> UnderApp [Test.Test]
editEssenceTest [] _ = return []
editEssenceTest (essence:rest) listData = do
  let fields = updateEssenceList essence $ fromJust (lookup essence listData)
  resultValue <- evalSApp dbEdit (EssenceList essence "edit" fields)
  let funcName = "editEssenceTest"
  let test = getTest (EssenceList essence "edit" []) funcName resultValue
  let label = funcName <> "_" <> essence
  (Test.TestLabel label test :) <$> editEssenceTest rest listData

getEssenceTest :: [String] -> EssenceData -> UnderApp [Test.Test]
getEssenceTest [] _ = return []
getEssenceTest (essence:rest) listData = do
  let fields = updateEssenceList essence $ fromJust (lookup essence listData)
  essenceValue <- evalSApp dbGet (EssenceList essence "get" fields)
  let funcName = "getEssenceTest"
  let test = getTest (EssenceList essence "get" fields) funcName essenceValue
  let label = funcName <> "_" <> essence
  (Test.TestLabel label test :) <$> getEssenceTest rest listData

getOneTest :: EssenceData -> UnderApp [Test.Test]
getOneTest listData = do
  let essence = "author"
  let pare =
        case lookup2 essence "id" listData of
          Just x -> ("id", x)
          Nothing -> ("id", MyEmpty)
  let essenceList = EssenceList essence "get" [pare]
  essenceValue <- dbGetOne essenceList
  let funcName = "getOneTest"
  let test = getTest essenceList funcName essenceValue
  let label = funcName <> "_" <> essence
  return [Test.TestLabel label test]

getArrayTest :: EssenceData -> UnderApp [Test.Test]
getArrayTest listData = do
  let essence = "tag"
  let pare =
        case lookup2 essence "id" listData of
          Just (MyInteger idNum) -> ("id", MyIntegers [idNum])
          _ -> ("id", MyEmpty)
  let essenceList = EssenceList essence "get" [pare]
  essenceValue <- dbGetArray essenceList
  let funcName = "getArrayTest"
  let test = getTest essenceList "dbGetArray" essenceValue
  let label = funcName <> "_" <> essence
  return [Test.TestLabel label test]

deleteEssenceTest :: [String] -> EssenceData -> UnderApp [Test.Test]
deleteEssenceTest [] _ = return []
deleteEssenceTest (essence:rest) listData = do
  let fields = updateEssenceList essence $ fromJust (lookup essence listData)
  resultValue <- evalSApp dbDelete (EssenceList essence "delete" fields)
  let funcName = "deleteEssenceTest"
  let test = getTest (EssenceList essence "delete" []) funcName resultValue
  let label = funcName <> "_" <> essence
  (Test.TestLabel label test :) <$> deleteEssenceTest rest listData

getNeededFields :: EssenceName -> A.Object -> EssenceList
getNeededFields "draft" obj =
  case getValue ["draft1"] obj of
    A.Object fieldsObj ->
      withoutEmpty . map (\(l, r) -> (T.unpack l, fromValue r)) $
      HM.toList fieldsObj
    _ -> []
getNeededFields essence obj =
  case getValue [essence <> "1", "id"] obj of
    A.Null -> []
    value -> [("id", fromValue value)]

updateData :: EssenceName -> A.Value -> S EssenceData UnderApp ()
updateData essence value = do
  let essenceObj =
        case value of
          A.Object obj -> obj
          _ -> HM.empty
  let fieldsData = getNeededFields essence essenceObj
  modifySApp $ (:) (T.unpack essence, fieldsData)

chooseNameForAdding :: String -> [String]
chooseNameForAdding name =
  case name of
    "author" -> ["person"]
    "draft" -> ["author", "category", "tag"]
    "news" -> ["draft"]
    "comment" -> ["person", "news"]
    _ -> [name]

getRelatedFields :: String -> EssenceData -> EssenceList
getRelatedFields "news" currentData =
  concat
    [ case lookup essence currentData of
      Just xs ->
        map
          (\(l, r) ->
             if l == "id"
               then ("draft_id", r)
               else (l, r))
          xs
      Nothing -> []
    | essence <- chooseNameForAdding "news"
    ]
getRelatedFields name currentData =
  concat
    [ case lookup essence currentData of
      Just xs -> map (\(l, r) -> (essence <> "_" <> l, r)) xs
      Nothing -> []
    | essence <- chooseNameForAdding name
    ]

handleDraftCase :: String -> EssenceList -> EssenceList
handleDraftCase name fields =
  case name of
    "draft" ->
      case lookup "tag_id" fields of
        Just (MyInteger idNum) ->
          ("tag_ids", MyIntegers [idNum]) :
          Data.Base.deletePair ("tag_id", MyEmpty) fields
        _ -> fields
    _ -> fields
