module Tests.Request where

import Network.Wai
import Network.Wai.Internal

------------------------------------------------------------------
---------------------------Test Request
------------------------------------------------------------------
testPersonCreateReq = testPostReq { pathInfo = personCreate, queryString = personCreateQueryString}
------------------------------------------------------------------
---------------------------Essence Query String
------------------------------------------------------------------
-- | Person
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