module Tests.Essence where

import Data.Essence
import Data.MyValue

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

--------------------------------------------------------------------------------------------
-----------------------------------Default Setting
defaultFirstName, defaultLastName, defaultDate, defaultAvatar, defaultAccessKey, defaultName, defaultContent ::
     T.Text
defaultFirstName = T.pack testLastName

defaultLastName = T.pack testFirstName

defaultAvatar = T.pack testAvatar

defaultDate = T.pack testDate

defaultAccessKey = T.pack testAccessKey

defaultName = T.pack testName

defaultContent = T.pack testContent

defaultHM :: A.Object
defaultHM = HM.fromList defaultList

defaultList :: [(T.Text, A.Value)]
defaultList =
  [ ("first_name", A.String defaultFirstName)
  , ("last_name", A.String defaultLastName)
  , ("avatar", A.String defaultAvatar)
  , ("date_of_creation", A.String defaultDate)
  , ("access_key", A.String defaultAccessKey)
  , ("name", A.String defaultName)
  , ("content", A.String defaultContent)
  , ("is_admin", A.Bool True)
  ]

--------------------------------------------------------------------------------------------
-----------------------------------Essence List
createEssenceList :: [Essence List]
createEssenceList =
  [ testPersonListCreate
  , testAuthorListCreate
  , testCategoryListCreate
  , testTagListCreate
  , testDraftListCreate
  , testNewsListCreate
  , testCommentListCreate
  ]

--------------------------------------------------------------------------------------------
testEssences :: [String]
testEssences =
  ["person", "author", "category", "tag", "draft", "news", "comment"]

testFirstName, testLastName, testAccessKey, testAvatar, testDate, testName, testContent ::
     String
testFirstName = "testFirstName"

testLastName = "testLastName"

testAccessKey = "12345678-1234-1234-1234-123456789abc"

testAvatar = "https://oppai-dragon.site/images/avatar.jpg"

testDate = "2020-08-06"

testName = "testName"

testContent = "testContent"

-- | Person
testPersonListCreate :: Essence List
testPersonListCreate = EssenceList "person" "create" testPersonListCreateFields

testPersonListCreateFields :: List
testPersonListCreateFields =
  [ ("first_name", MyString testFirstName)
  , ("last_name", MyString testLastName)
  , ("date_of_creation", MyDate testDate)
  , ("avatar", MyString testAvatar)
  , ("access_key", MyString testAccessKey)
  , ("is_admin", MyBool True)
  ]

-- | Author
testAuthorListCreate :: Essence List
testAuthorListCreate = EssenceList "author" "create" testAuthorListCreateFields

testAuthorListCreateFields :: List
testAuthorListCreateFields = []

-- | Category
testCategoryListCreate :: Essence List
testCategoryListCreate =
  EssenceList "category" "create" testCategoryListCreateFields

testCategoryListCreateFields :: List
testCategoryListCreateFields = [("name", MyString testName)]

-- | Tag
testTagListCreate :: Essence List
testTagListCreate = EssenceList "tag" "create" testTagListCreateFields

testTagListCreateFields :: List
testTagListCreateFields = [("name", MyString testName)]

-- | Draft
testDraftListCreate :: Essence List
testDraftListCreate = EssenceList "draft" "create" testDraftListCreateFields

testDraftListCreateFields :: List
testDraftListCreateFields =
  [ ("name", MyString testName)
  , ("content", MyString testContent)
  , ("date_of_creation", MyDate testDate)
  ]

-- | News
testNewsListCreate :: Essence List
testNewsListCreate = EssenceList "news" "create" testNewsListCreateFields

testNewsListCreateFields :: List
testNewsListCreateFields = [("date_of_creation", MyDate testDate)]

-- For functions without access to database
testNewsCreateFields :: List
testNewsCreateFields = [("id", MyInteger 1), ("content", MyString "kek")]

testNewsEditFields :: List
testNewsEditFields = [("id", MyInteger 1), ("content", MyString "kek")]

testNewsGetFields :: List
testNewsGetFields =
  [ ("id", MyInteger 1)
  , ("filter_author_name", MyString "misha dragon")
  , ("search_category_name", MyString "cat")
  , ("sort_date_of_creation", MyBool True)
  ]

testNewsDeleteFields :: List
testNewsDeleteFields = [("id", MyInteger 1)]

-- | Comment
testCommentListCreate :: Essence List
testCommentListCreate =
  EssenceList "comment" "create" testCommentListCreateFields

testCommentListCreateFields :: List
testCommentListCreateFields =
  [("content", MyString testContent), ("date_of_creation", MyDate testDate)]

--------------------------------------------------------------------------------------------
-----------------------------------Essence Database
--------------------------------------------------------------------------------------------
-- | Person
testPersonDatabase :: Essence Database
testPersonDatabase =
  EssenceDatabase "person" $ HM.fromList testPersonDatabaseFields

testPersonDatabaseFields :: [(String, [(String, String)])]
testPersonDatabaseFields =
  zip testPersonDatabaseFieldsName testPersonDatabaseDescription

testPersonDatabaseFieldsName :: [String]
testPersonDatabaseFieldsName =
  [ "id"
  , "avatar"
  , "date_of_creation"
  , "is_admin"
  , "access_key"
  , "last_name"
  , "first_name"
  ]

testPersonDatabaseDescription :: [[(String, String)]]
testPersonDatabaseDescription =
  [ [("type", "int"), ("constraint", "primary key")]
  , [("type", "string"), ("value", "null")]
  , [("type", "date"), ("value", "null")]
  , [("type", "bool"), ("value", "null")]
  , [("type", "uuid"), ("value", "null")]
  , [("type", "string"), ("value", "not null")]
  , [("type", "string"), ("value", "not null")]
  ]

--------------------------------------------------------------------------------------------
-----------------------------------Essence DB
--------------------------------------------------------------------------------------------
-- | Author
testAuthorGetDB :: Essence DB
testAuthorGetDB = EssenceDB "author" "get" testAuthorHMDescription

testAuthorHMDescription :: DB
testAuthorHMDescription = HM.fromList testAuthorDBFields

testAuthorDBFields :: [(String, Description)]
testAuthorDBFields = zip testAuthorDBFieldsName testAuthorDBDescription

testAuthorDBFieldsName :: [String]
testAuthorDBFieldsName = ["id", "person_id", "description"]

testAuthorDBDescription :: [Description]
testAuthorDBDescription =
  [ Description (MyInteger 0) Nothing Nothing (Just PRIMARY)
  , Description
      (MyInteger 0)
      (Just $ NOT NULL)
      (Just $ Relations "person" "id")
      (Just UNIQUE)
  , Description (MyString "") Nothing Nothing Nothing
  ]

-- | Person
testPersonCreateDB :: Essence DB
testPersonCreateDB = EssenceDB "person" "create" testPersonHMDescription

testPersonHMDescription :: DB
testPersonHMDescription = HM.fromList testPersonDBFields

testPersonDBFields :: [(String, Description)]
testPersonDBFields = zip testPersonDBFieldsName testPersonDBDescription

testPersonDBFieldsName :: [String]
testPersonDBFieldsName =
  ["id", "avatar", "date_of_creation", "is_admin", "last_name", "first_name", "access_key"]

testPersonDBDescription :: [Description]
testPersonDBDescription =
  [ Description (MyInteger 0) Nothing Nothing (Just PRIMARY)
  , Description (MyString "") (Just NULL) Nothing Nothing
  , Description (MyDate "") (Just NULL) Nothing Nothing
  , Description (MyBool False) (Just NULL) Nothing Nothing
  , Description (MyString "") (Just $ NOT NULL) Nothing Nothing
  , Description (MyString "") (Just $ NOT NULL) Nothing Nothing
  , Description (MyString "") (Just $ NOT NULL) Nothing Nothing
  ]
