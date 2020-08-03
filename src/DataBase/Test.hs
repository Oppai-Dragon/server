module Database.Test
    ( EssenceData
    , EssenceName
    , getNeededFields
    , updateData
    , chooseNameForAdding
    , getRelatedFields
    , handleDraftCase
    , getRandomName
    , handleUniqueName
    ) where

import Config
import qualified Data.Base as Base
import Data.Essence.Methods
import Data.MyValue

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Class          (lift)

type EssenceList = [(String,MyValue)]
type EssenceData = [(String,EssenceList)]
type EssenceName = T.Text

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
            : Base.deletePair ("tag_id",MyEmpty) fields
        _                   -> fields
    _       -> fields

getRandomName :: String -> IO String
getRandomName name = do
    randomNum <- Base.getRandom
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
