module Tests.Essence where

import Data.Essence
import Data.Essence.Column
import Data.MyValue

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

--import qualified Data.Vector as V
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
testCategoryListCreateFields = []

-- | Tag
testTagListCreate :: Essence List
testTagListCreate = EssenceList "tag" "create" testTagListCreateFields

testTagListCreateFields :: List
testTagListCreateFields = []

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
testNewsListCreateFields = []

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
-----------------------------------Essence Column
--------------------------------------------------------------------------------------------
-- | Author
testAuthorGetColumn :: Essence Column
testAuthorGetColumn = EssenceColumn "author" "get" testAuthorHMColumn

testAuthorHMColumn :: HM.HashMap String Column
testAuthorHMColumn = HM.fromList testAuthorColumnFields

testAuthorColumnFields :: [(String, Column)]
testAuthorColumnFields = zip testAuthorColumnFieldsName testAuthorColumnList

testAuthorColumnFieldsName :: [String]
testAuthorColumnFieldsName = ["id", "person_id", "Column"]

testAuthorColumnList :: [Column]
testAuthorColumnList =
  [ defaultColumn {cValueType = BIGSERIAL, cConstraint = Just PrimaryKey}
  , defaultColumn
      { cValueType = BIGINT
      , cNULL = Just $ NOT NULL
      , cRelations = Just $ Relations "person" "id"
      , cConstraint = Just UNIQUE
      }
  , defaultColumn {cValueType = VARCHAR (150)}
  ]

-- | Person
testPersonCreateColumn :: Essence Column
testPersonCreateColumn = EssenceColumn "person" "create" testPersonHMColumn

testPersonHMColumn :: HM.HashMap String Column
testPersonHMColumn = HM.fromList testPersonColumnFields

testPersonColumnFields :: [(String, Column)]
testPersonColumnFields = zip testPersonColumnFieldsName testPersonColumnList

testPersonColumnFieldsName :: [String]
testPersonColumnFieldsName =
  ["id", "avatar", "date_of_creation", "is_admin", "last_name", "first_name"]

testPersonColumnList :: [Column]
testPersonColumnList =
  [ defaultColumn {cValueType = BIGSERIAL, cConstraint = Just PrimaryKey}
  , defaultColumn {cValueType = VARCHAR 50, cNULL = Just NULL}
  , defaultColumn {cValueType = DATE, cNULL = Just NULL}
  , defaultColumn
      {cValueType = BOOLEAN, cNULL = Just NULL, cConstraint = Just PrimaryKey}
  , defaultColumn {cValueType = VARCHAR 50, cNULL = Just $ NOT NULL}
  , defaultColumn {cValueType = VARCHAR 50, cNULL = Just $ NOT NULL}
  ]
