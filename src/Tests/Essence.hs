module Tests.Essence where

import Data.MyValue
import Data.Essence hiding (name)

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import qualified Data.Text           as T

import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------------------
-----------------------------------Default Setting
defaultFirstName, defaultLastName, defaultDate,
    defaultAvatar, defaultAccessKey, defaultName,
        defaultContent :: T.Text
defaultFirstName = T.pack lastName
defaultLastName  = T.pack firstName
defaultAvatar    = T.pack avatar
defaultDate      = T.pack date
defaultAccessKey = T.pack accessKey
defaultName      = T.pack name
defaultContent   = T.pack content

defaultHM :: Object
defaultHM = HM.fromList defaultList
defaultList :: [(T.Text,Value)]
defaultList =
    [("first_name",String defaultFirstName)
    ,("last_name",String defaultLastName)
    ,("avatar",String defaultAvatar)
    ,("date_of_creation",String defaultDate)
    ,("access_key",String defaultAccessKey)
    ,("name",String defaultName)
    ,("content",String defaultContent)
    ,("is_admin",Bool True)]
--------------------------------------------------------------------------------------------
-----------------------------------Essence List
createEssenceList =
    [testPersonListCreate
    ,testAuthorListCreate
    ,testCategoryListCreate
    ,testTagListCreate
    ,testDraftListCreate
    ,testNewsListCreate
    ,testCommentListCreate
    ]
--------------------------------------------------------------------------------------------
essences = ["person","author","category","tag","draft","news","comment"]
firstName, lastName, accessKey,
    avatar, date, name,
        content :: String
firstName = "testFirstName"
lastName  = "testLastName"
accessKey = "12345678-1234-1234-1234-123456789abc"
avatar    = "https://oppai-dragon.site/images/avatar.jpg"
date      = "2020-08-06"
name      = "testName"
content   = "testContent"
-- | Person
testPersonListCreate = EssenceList "person" "create" testPersonListCreateFields
testPersonListCreateFields =
    [("first_name",MyString firstName)
    ,("last_name",MyString lastName)
    ,("date_of_creation",MyDate date)
    ,("avatar",MyString avatar)
    ,("access_key",MyString accessKey)
    ,("is_admin",MyBool True)]
-- | Author
testAuthorListCreate = EssenceList "author" "create" testAuthorListCreateFields
testAuthorListCreateFields = []
-- | Category
testCategoryListCreate = EssenceList "category" "create" testCategoryListCreateFields
testCategoryListCreateFields = [("name",MyString name)]
-- | Tag
testTagListCreate = EssenceList "tag" "create" testTagListCreateFields
testTagListCreateFields = [("name",MyString name)]
-- | Draft
testDraftListCreate = EssenceList "draft" "create" testDraftListCreateFields
testDraftListCreateFields =
    [("name",MyString name)
    ,("content",MyString content)
    ,("date_of_creation",MyDate date)]
-- | News
testNewsListCreate = EssenceList "news" "create" testNewsListCreateFields
testNewsListCreateFields = [("date_of_creation",MyDate date)]
-- For functions without access to database
testNewsCreateFields = [("id",MyInteger 1),("content",MyString "kek")]
testNewsEditFields = [("id",MyInteger 1),("content",MyString "kek")]
testNewsGetFields =
    [("id",MyInteger 1)
    ,("filter_author_name",MyString "misha dragon")
    ,("search_category_name",MyString "cat")
    ,("sort_date_of_creation",MyBool True)]
testNewsDeleteFields = [("id",MyInteger 1)]
-- | Comment
testCommentListCreate = EssenceList "comment" "create" testCommentListCreateFields
testCommentListCreateFields =
    [("content",MyString content)
    ,("date_of_creation",MyDate date)]
--------------------------------------------------------------------------------------------
-----------------------------------Essence Database
--------------------------------------------------------------------------------------------
-- | Person
testPersonDatabase = EssenceDatabase "person"
    $ HM.fromList testPersonDatabaseFields
testPersonDatabaseFields =
    zip testPersonDatabaseFieldsName testPersonDatabaseDescription
testPersonDatabaseFieldsName =
    [ "id"
    , "avatar"
    , "date_of_creation"
    , "is_admin"
    , "access_key"
    , "last_name"
    , "first_name"
    ]
testPersonDatabaseDescription =
    [[("type","int"),("constraint","primary key")]
    ,[("type","string"),("value","null")]
    ,[("type","date"),("value","null")]
    ,[("type","bool"),("value","null")]
    ,[("type","uuid"),("value","null")]
    ,[("type","string"),("value","not null")]
    ,[("type","string"),("value","not null")]
    ]
--------------------------------------------------------------------------------------------
-----------------------------------Essence DB
--------------------------------------------------------------------------------------------
-- | Author
testAuthorGetDB = EssenceDB "author" "get" testAuthorHMDescription
testAuthorHMDescription = HM.fromList testAuthorDBFields
testAuthorDBFields = zip testAuthorDBFieldsName testAuthorDBDescription
testAuthorDBFieldsName =
    [ "id"
    , "person_id"
    , "description"
    ]
testAuthorDBDescription =
    [ Description (MyInteger 0) Nothing Nothing (Just PRIMARY)
    , Description (MyInteger 0) (Just $ NOT NULL) (Just $ Relations "person" "id") (Just UNIQUE)
    , Description (MyString "") Nothing Nothing Nothing
    ]

-- | Person
testPersonCreateDB = EssenceDB "person" "create" testPersonHMDescription
testPersonHMDescription = HM.fromList testPersonDBFields
testPersonDBFields = zip testPersonDBFieldsName testPersonDBDescription
testPersonDBFieldsName =
    [ "id"
    , "avatar"
    , "date_of_creation"
    , "is_admin"
    , "last_name"
    , "first_name"
    ]
testPersonDBDescription =
    [ Description (MyInteger 0) Nothing             Nothing (Just PRIMARY)
    , Description (MyString "") (Just NULL)         Nothing Nothing
    , Description (MyDate "") (Just $ NULL)     Nothing Nothing
    , Description (MyBool False) (Just NULL)        Nothing Nothing
    , Description (MyString "") (Just $ NOT NULL)   Nothing Nothing
    , Description (MyString "") (Just $ NOT NULL)   Nothing Nothing
    ]
