module Tests.Request where

import Data.Request

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

import Network.Wai
import Network.Wai.Internal

import Test.HUnit

requestTests =
    [ TestLabel "queryFromObjTest"                       queryFromObjTest
    , TestLabel "getQueryString_fromQueryString_Test"    getQueryString_fromQueryString_Test
    , TestLabel "getQueryString_fromJson_Test"           getQueryString_fromJson_Test
    ]

queryFromObjTest =
    TestCase $
    assertEqual
    "for (queryFromObj testObj)"
    [("first_name",Just "misha")
    ,("access_key",Just "")
    ,("is_admin",Just "True")
    ,("id",Just "1")
    ,("tag_ids",Just "[1,2]")
    ,("optional_photos",Just "[uri1,uri2]")]
    $ queryFromObj testObj

getQueryString_fromQueryString_Test =
    TestCase $
    getQueryString testPersonCreateReq
    >>= assertEqual "for (getQueryString testPersonCreateReq)"
    personCreateQueryString

getQueryString_fromJson_Test =
    TestCase $
    getQueryString testPersonCreateReqJson
    >>= assertEqual "for (getQueryString testPersonCreateReqJson)"
    personCreateQueryString

testObj = HM.fromList
    [("first_name",String "misha")
    ,("tag_ids",Array $ V.fromList [Number 1,Number 2])
    ,("optional_photos",Array $ V.fromList [String "uri1",String "uri2"])
    ,("id",Number 1)
    ,("is_admin",Bool True)
    ,("access_key",Null)
    ]

testPersonCreateReqJson = testPostReq { pathInfo = personCreate, requestBody = return personCreateBSJson, requestHeaders = [("Content-Type", "application/json")]}
------------------------------------------------------------------
---------------------------Test Request
------------------------------------------------------------------
testPersonCreateReq = testPostReq { pathInfo = personCreate, queryString = personCreateQueryString}
testAuthorCreateReq = testPostReq { pathInfo = authorCreate, queryString = authorCreateQueryString}
------------------------------------------------------------------
---------------------------Essence Query String
------------------------------------------------------------------
-- | Person
personCreateBSJson      = "{\"first_name\":\"misha\",\"last_name\":\"dragon\"}"
personCreateQueryString = [("first_name",Just "misha"),("last_name",Just "dragon")]
personGetQueryString    = []
personDeleteQueryString = [("access_key",Just "key"),("id",Just "1")]   ------------ HOW GET THAT ACCESS KEY??
-- | Author
authorCreateQueryString = [("access_key",Just "key")]
authorEditQueryString   = [("",Just "")]
authorGetQueryString    = [("",Just "")]
authorDeleteQueryString = [("",Just "")]
-- | Category
categoryCreateQueryString = [("",Just "")]
categoryEditQueryString   = [("",Just "")]
categoryGetQueryString    = [("",Just "")]
categoryDeleteQueryString = [("",Just "")]
-- | Tag
tagCreateQueryString = [("",Just "")]
tagEditQueryString   = [("",Just "")]
tagGetQueryString    = [("",Just "")]
tagDeleteQueryString = [("",Just "")]
-- | Draft
draftCreateQueryString  = [("",Just "")]
draftEditQueryString    = [("",Just "")]
draftGetQueryString     = [("",Just "")]
draftDeleteQueryString  = [("",Just "")]
draftPublishQueryString = [("",Just "")]
-- | News
newsGetQueryString    = [("",Just "")]
-- | Comment
commentCreateQueryString = [("",Just "")]
commentEditQueryString   = [("",Just "")]
commentGetQueryString    = [("",Just "")]
commentDeleteQueryString = [("",Just "")]
------------------------------------------------------------------
---------------------------Essence Action
------------------------------------------------------------------
-- | Person
personCreate = [person, create]
personGet    = [person, get]
personDelete = [person, delete]
-- | Author
authorCreate = [author, create]
authorEdit   = [author, edit]
authorGet    = [author, get]
authorDelete = [author, delete]
-- | Category
categoryCreate = [category, create]
categoryEdit   = [category, edit]
categoryGet    = [category, get]
categoryDelete = [category, delete]
-- | Tag
tagCreate = [tag, create]
tagEdit   = [tag, edit]
tagGet    = [tag, get]
tagDelete = [tag, delete]
-- | Draft
draftCreate  = [draft, create]
draftEdit    = [draft, edit]
draftGet     = [draft, get]
draftDelete  = [draft, delete]
draftPublish = [draft, publish]
-- | News
newsGet    = [news, get]
-- | Comment
commentCreate = [comment, create]
commentEdit   = [comment, edit]
commentGet    = [comment, get]
commentDelete = [comment, delete]
------------------------------------------------------------------
---------------------------Action
------------------------------------------------------------------
create   = "create"
edit     = "edit"
get      = "get"
delete   = "delete"
publish  = "publish"
------------------------------------------------------------------
---------------------------Essence
------------------------------------------------------------------
person   = "person"
author   = "author"
category = "category"
tag      = "tag"
draft    = "draft"
news     = "news"
comment  = "comment"
------------------------------------------------------------------
---------------------------Method Request
------------------------------------------------------------------
testGetReq  = defaultRequest
testPostReq = defaultRequest { requestMethod = "POST" }