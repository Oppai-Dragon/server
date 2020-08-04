module Tests.Essence where

import Data.MyValue
import Data.Essence

import Data.Aeson
import qualified Data.HashMap.Strict as HM

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
accessKeyStr = "12345678-1234-1234-1234-123456789abc"
-- | Person
testPersonListCreate = EssenceList "person" "create" testPersonListCreateFields
testPersonListCreateFields =
    [("first_name",MyString "testFirstName")
    ,("last_name",MyString "testLastName")
    ,("avatar",MyString "uri")
    ,("access_key",MyString accessKeyStr)
    ,("is_admin",MyBool True)]
-- | Author
testAuthorListCreate = EssenceList "author" "create" testAuthorListCreateFields
testAuthorListCreateFields = []
-- | Category
testCategoryListCreate = EssenceList "category" "create" testCategoryListCreateFields
testCategoryListCreateFields = []
-- | Tag
testTagListCreate = EssenceList "tag" "create" testTagListCreateFields
testTagListCreateFields = []
-- | Draft
testDraftListCreate = EssenceList "draft" "create" testDraftListCreateFields
testDraftListCreateFields =
    [("name",MyString "testDraft")
    ,("content",MyString "testContent")]
-- | News
testNewsListCreate = EssenceList "news" "create" testNewsListCreateFields
testNewsListCreateFields = []
-- For functions without access to database
testNewsCreateFields = [("id",MyInteger 1),("content",MyString "kek")]
testNewsEditFields = [("id",MyInteger 1),("content",MyString "kek")]
testNewsGetFields =
    [("id",MyInteger 1)
    ,("filter_author_name",MyString "misha dragon")
    ,("search_category_name",MyString "cat")
    ,("sort",MyString "date_of_creation")]
testNewsDeleteFields = [("id",MyInteger 1)]
-- | Comment
testCommentListCreate = EssenceList "comment" "create" testCommentListCreateFields
testCommentListCreateFields = [("content",MyString "privet")]
--------------------------------------------------------------------------------------------
-----------------------------------Essence Database
--------------------------------------------------------------------------------------------
-- | Person
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
testAuthorGetDB = EssenceDB "author" "get" $ HM.fromList
    [("id",Description (MyInteger 0) Nothing Nothing (Just PRIMARY))
    ,("person_id",Description (MyInteger 0) (Just $ NOT NULL) (Just $ Relations "person" "id") (Just UNIQUE))
    ,("description",Description (MyString "") Nothing Nothing Nothing)
    ]

-- | Person
testPersonCreateDB = EssenceDB "person" "create" $ HM.fromList testPersonCreateDBFields
testPersonCreateDBFields = zip testPersonCreateDBFieldsName testPersonCreateDBDescription
testPersonCreateDBFieldsName =
    [ "id"
    , "avatar"
    , "date_of_creation"
    , "is_admin"
    , "last_name"
    , "first_name"
    ]
testPersonCreateDBDescription =
    [ Description (MyInteger 0) Nothing             Nothing (Just PRIMARY)
    , Description (MyString "") (Just NULL)         Nothing Nothing
    , Description (MyDate "") (Just $ NULL)     Nothing Nothing
    , Description (MyBool False) (Just NULL)        Nothing Nothing
    , Description (MyString "") (Just $ NOT NULL)   Nothing Nothing
    , Description (MyString "") (Just $ NOT NULL)   Nothing Nothing
    ]
