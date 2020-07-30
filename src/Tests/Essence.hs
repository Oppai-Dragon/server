module Tests.Essence where

import Data.MyValue
import Data.Essence

import qualified Data.HashMap.Strict as HM

testEssenceList = EssenceList "person" "create" testEssenceListFields
testEssenceListFields = [("first_name",MyString "misha"),("last_name",MyString "dragon")]
testCreateList = [("id",MyInteger 1),("content",MyString "kek")]
testEditList = [("id",MyInteger 1),("content",MyString "kek")]
testGetList =
    [("id",MyInteger 1)
    ,("filter_author_name",MyString "misha dragon")
    ,("search_category_name",MyString "cat")
    ,("sort",MyString "date_of_creation")]
testDeleteTest = [("id",MyInteger 1)]

testEssenceDatabase = EssenceDatabase "person" "create"
    $ HM.fromList testEssenceDatabaseFields
testEssenceDatabaseFields =
    zip testEssemceDatabaseFieldsName testEssenceDatabaseDescription
testEssemceDatabaseFieldsName =
    [ "id"
    , "avatar"
    , "date_of_creation"
    , "is_admin"
    , "access_key"
    , "last_name"
    , "first_name"
    ]
testEssenceDatabaseDescription =
    [[("type","int"),("constraint","primary key")]
    ,[("type","string"),("value","null")]
    ,[("type","date"),("value","not null")]
    ,[("type","bool"),("value","null")]
    ,[("type","uuid"),("value","null")]
    ,[("type","string"),("value","not null")]
    ,[("type","string"),("value","not null")]
    ]

testEssenceDB = EssenceDB "person" "create" $ HM.fromList testEssenceDBFields
testEssenceDBFields = zip testEssemceDBFieldsName testEssenceDBDescription
testEssemceDBFieldsName =
    [ "id"
    , "avatar"
    , "date_of_creation"
    , "is_admin"
    , "last_name"
    , "first_name"
    ]
testEssenceDBDescription =
    [ Description (MyInteger 0) Nothing             Nothing (Just PRIMARY)
    , Description (MyString "") (Just NULL)         Nothing Nothing
    , Description (MyDate "") (Just $ NOT NULL)     Nothing Nothing
    , Description (MyBool False) (Just NULL)        Nothing Nothing
    , Description (MyString "") (Just $ NOT NULL)   Nothing Nothing
    , Description (MyString "") (Just $ NOT NULL)   Nothing Nothing
    ]
