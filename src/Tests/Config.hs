module Tests.Config
    ( configTests
    ) where

import Config
import Data.Request.Access
import Data.Essence.RelationsTree

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V
import Data.Text
import Test.HUnit

configTests =
    [ TestLabel "parsePathTest"             parsePathTest
    , TestLabel "getApiActionsTest"         getApiActionsTest
    , TestLabel "getEssenceFieldsTest"      getEssenceFieldsTest
    , TestLabel "parseFieldsFuncTest"       parseFieldsFuncTest
    , TestLabel "getEssencesTest"           getEssencesTest
    , TestLabel "getActionsForEssenceTest"  getActionsForEssenceTest
    , TestLabel "getAccessTest"             getAccessTest
    , TestLabel "getActionsAccessTest"      getActionsAccessTest
    , TestLabel "getUriTest"                getUriTest
    , TestLabel "getUriDBTest"              getUriDBTest
    , TestLabel "getMethodActionsTest"      getMethodActionsTest
    , TestLabel "getApiDBMethodTest"        getApiDBMethodTest
    , TestLabel "getRelationsTreeTest"      getRelationsTreeTest
    , TestLabel "getRelationsTree'Test"     getRelationsTree'Test
    , TestLabel "getOffsetLimitTest"        getOffsetLimitTest
    ]

parsePathTest =
    TestCase $
    (parsePath <*> pure "C:Repo\\server\\src") >>=
    assertEqual "for (parsePath \"C:Repo\\server\\src\")"
    "C:Repo"

getApiActionsTest =
    TestCase $
    assertEqual "for (getApiActions testApi)"
    ["create","edit","get","delete","puglish"]
    $ getApiActions testApi

getEssenceFieldsTest =
    TestCase $
    assertEqual
    "for (getEssenceFields \"category\" testConfig)"
    ["id","name", "childs_category"]
    $ getEssenceFields "category" testConfig

parseFieldsFuncTest =
    TestCase $
    assertEqual "for (parseMaybe (parseFieldsFunc [\"essences\"]) testApi)"
    (Just . Array $ V.fromList
    ["person","author","category","tag","draft","news","comment"])
    $ parseMaybe (parseFieldsFunc ["essences"]) testApi

getEssencesTest =
    TestCase $
    assertEqual
    "for (getEssencesTest testApi)"
    ["person","author","category","tag","draft","news","comment"]
    $ getEssences testApi

getActionsForEssenceTest =
    TestCase $
    assertEqual
    "for (getActionsForEssence \"category\" testApi)"
    ["edit","get","create","delete"]
    $ getActionsForEssence "category" testApi

getAccessTest =
    TestCase $
    assertEqual "for (getAccess \"draft\" \"publish\" testApi)"
    Author
    $ getAccess "draft" "publish" testApi

getActionsAccessTest =
    TestCase $
    assertEqual
    "for (getActionsAccessTest \"category\" testApi)"
    [("edit", Admin)
    ,("get", Everyone)
    ,("create", Admin)
    ,("delete", Admin)]
    $ getActionsAccess "category" testApi

getUriTest =
    TestCase $
    assertEqual "for (getUri testConfig)"
    "postgresql://postgres:0000@localhost:5432/webserv"
    $ getUri testConfig

getUriDBTest =
    TestCase $
    assertEqual "for (getUriDB testPsql)"
    "postgresql://postgres:0000@localhost:5432/webserv"
    $ getUriDB testPsql

getMethodActionsTest =
    TestCase $
    assertEqual "for (getMethodActions \"POST\" testApi)"
    ["create","edit","delete","publish"]
    $ getMethodActions "POST" testApi

getApiDBMethodTest =
    TestCase $
    assertEqual "for (getApiDBMethod \"publish\" testApi)"
    "create"
    $ getApiDBMethod "publish" testApi

getRelationsTreeTest =
    TestCase $
    assertEqual "for (getRelationsTree \"draft\" testApi)"
    (Root "person_id"
    $ Trunk "author_id"
    $ Branch "draft" [Leaf "author_id"]
    ) $ getRelationsTree "draft" testApi

getRelationsTree'Test =
    TestCase $
    assertEqual "for (getRelationsTree' \"draft\" 1 \"draft\" testApi)"
    (Root "person_id"
    $ Trunk "author_id"
    $ Branch "draft" [Leaf "author_id"]
    ) $ getRelationsTree' "draft" 1 "draft" testApi

getOffsetLimitTest =
    TestCase $
    assertEqual
    "for (getOffsetLimit 1 testPsql)"
    "OFFSET 0 LIMIT 10"
    $ getOffsetLimit 1 testPsql