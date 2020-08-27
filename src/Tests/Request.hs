module Tests.Request where

import Data.Request

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai

import Test.HUnit

requestTests :: [Test]
requestTests =
  [ TestLabel "queryFromObjTest" queryFromObjTest
  , TestLabel
      "getQueryStringFromQueryStringTest"
      getQueryStringFromQueryStringTest
  ]

queryFromObjTest, getQueryStringFromQueryStringTest :: Test
queryFromObjTest =
  TestCase $
  assertEqual
    "for (queryFromObj testObj)"
    [ ("first_name", Just "misha")
    , ("access_key", Just "")
    , ("is_admin", Just "True")
    , ("id", Just "1")
    , ("tag_ids", Just "[1,2]")
    , ("optional_photos", Just "[uri1,uri2]")
    ] $
  queryFromObj testObj

getQueryStringFromQueryStringTest =
  TestCase $
  getQueryString testPersonCreateReq >>=
  assertEqual "for (getQueryString testPersonCreateReq)" personCreateQueryString

testObj :: A.Object
testObj =
  HM.fromList
    [ ("first_name", A.String "misha")
    , ("tag_ids", A.Array $ V.fromList [A.Number 1, A.Number 2])
    , ( "optional_photos"
      , A.Array $ V.fromList [A.String "uri1", A.String "uri2"])
    , ("id", A.Number 1)
    , ("is_admin", A.Bool True)
    , ("access_key", A.Null)
    ]

------------------------------------------------------------------
---------------------------Test Request
------------------------------------------------------------------
testPersonCreateReq, testAuthorCreateReq :: Wai.Request
testPersonCreateReq =
  testPostReq
    {Wai.pathInfo = personCreate, Wai.queryString = personCreateQueryString}

testAuthorCreateReq =
  testPostReq
    {Wai.pathInfo = authorCreate, Wai.queryString = authorCreateQueryString}

------------------------------------------------------------------
---------------------------Essence Query String
------------------------------------------------------------------
-- | Person
personCreateQueryString, personGetQueryString, personDeleteQueryString ::
     HTTPTypes.Query
personCreateQueryString =
  [("first_name", Just "misha"), ("last_name", Just "dragon")]

personGetQueryString = []

personDeleteQueryString = [("access_key", Just "key"), ("id", Just "1")] ------------ HOW GET THAT ACCESS KEY??

-- | Author
authorCreateQueryString, authorEditQueryString, authorGetQueryString, authorDeleteQueryString ::
     HTTPTypes.Query
authorCreateQueryString = [("access_key", Just "key")]

authorEditQueryString = [("", Just "")]

authorGetQueryString = [("", Just "")]

authorDeleteQueryString = [("", Just "")]

-- | Category
categoryCreateQueryString, categoryEditQueryString, categoryGetQueryString, categoryDeleteQueryString ::
     HTTPTypes.Query
categoryCreateQueryString = [("", Just "")]

categoryEditQueryString = [("", Just "")]

categoryGetQueryString = [("", Just "")]

categoryDeleteQueryString = [("", Just "")]

-- | Tag
tagCreateQueryString, tagEditQueryString, tagGetQueryString, tagDeleteQueryString ::
     HTTPTypes.Query
tagCreateQueryString = [("", Just "")]

tagEditQueryString = [("", Just "")]

tagGetQueryString = [("", Just "")]

tagDeleteQueryString = [("", Just "")]

-- | Draft
draftCreateQueryString, draftEditQueryString, draftGetQueryString, draftDeleteQueryString, draftPublishQueryString ::
     HTTPTypes.Query
draftCreateQueryString = [("", Just "")]

draftEditQueryString = [("", Just "")]

draftGetQueryString = [("", Just "")]

draftDeleteQueryString = [("", Just "")]

draftPublishQueryString = [("", Just "")]

-- | News
newsGetQueryString :: HTTPTypes.Query
newsGetQueryString = [("", Just "")]

-- | Comment
commentCreateQueryString, commentEditQueryString, commentGetQueryString, commentDeleteQueryString ::
     HTTPTypes.Query
commentCreateQueryString = [("", Just "")]

commentEditQueryString = [("", Just "")]

commentGetQueryString = [("", Just "")]

commentDeleteQueryString = [("", Just "")]

------------------------------------------------------------------
---------------------------Essence Action
------------------------------------------------------------------
-- | Person
personCreate, personGet, personDelete :: [T.Text]
personCreate = [person, create]

personGet = [person, get]

personDelete = [person, delete]

-- | Author
authorCreate, authorEdit, authorGet, authorDelete :: [T.Text]
authorCreate = [author, create]

authorEdit = [author, edit]

authorGet = [author, get]

authorDelete = [author, delete]

-- | Category
categoryCreate, categoryEdit, categoryGet, categoryDelete :: [T.Text]
categoryCreate = [category, create]

categoryEdit = [category, edit]

categoryGet = [category, get]

categoryDelete = [category, delete]

-- | Tag
tagCreate, tagEdit, tagGet, tagDelete :: [T.Text]
tagCreate = [tag, create]

tagEdit = [tag, edit]

tagGet = [tag, get]

tagDelete = [tag, delete]

-- | Draft
draftCreate, draftEdit, draftGet, draftDelete, draftPublish :: [T.Text]
draftCreate = [draft, create]

draftEdit = [draft, edit]

draftGet = [draft, get]

draftDelete = [draft, delete]

draftPublish = [draft, publish]

-- | News
newsGet :: [T.Text]
newsGet = [news, get]

-- | Comment
commentCreate, commentEdit, commentGet, commentDelete :: [T.Text]
commentCreate = [comment, create]

commentEdit = [comment, edit]

commentGet = [comment, get]

commentDelete = [comment, delete]

------------------------------------------------------------------
---------------------------Action
------------------------------------------------------------------
create, edit, get, delete, publish :: T.Text
create = "create"

edit = "edit"

get = "get"

delete = "delete"

publish = "publish"

------------------------------------------------------------------
---------------------------Essence
------------------------------------------------------------------
person, author, category, tag, draft, news, comment :: T.Text
person = "person"

author = "author"

category = "category"

tag = "tag"

draft = "draft"

news = "news"

comment = "comment"

------------------------------------------------------------------
---------------------------Method Request
------------------------------------------------------------------
testGetReq :: Wai.Request
testGetReq = Wai.defaultRequest

testPostReq :: Wai.Request
testPostReq = Wai.defaultRequest {Wai.requestMethod = "POST"}
