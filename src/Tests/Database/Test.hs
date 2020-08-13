module Tests.Database.Test
    ( databaseTestTests
    ) where

import Config

import Data.Essence
import Data.MyValue
import Database.Test

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS

import Tests.Essence

import Test.HUnit

databaseTestTests =
    [ TestLabel "getDefaultValuesTest"          getDefaultValuesTest
    , TestLabel "build_create_EssenceValueTest" build_create_EssenceValueTest
    , TestLabel "build_edit_EssenceValueTest"   build_edit_EssenceValueTest
    , TestLabel "build_get_EssenceValueTest"    build_get_EssenceValueTest
    , TestLabel "build_delete_EssenceValueTest" build_delete_EssenceValueTest
    , TestLabel "getNeededFields_draft_Test"    getNeededFields_draft_Test
    , TestLabel "getNeededFields_person_Test"   getNeededFields_person_Test
    , TestLabel "updateDataTest"                updateDataTest
    , TestLabel "get_news_RelatedFieldsTest"    get_news_RelatedFieldsTest
    , TestLabel "get_other_RelatedFieldsTest"   get_other_RelatedFieldsTest
    , TestLabel "handleDraftCaseTest"           handleDraftCaseTest
    ] <> chooseNameForAddingTests

getDefaultValuesTest =
    TestCase $
    assertEqual
    "for (getDefaultValues [\"first_name\",\"last_name\",\"date_of_creation\",\"avatar\",\"is_admin\",\"access_key\"])"
    [("first_name",String defaultFirstName)
    ,("last_name",String defaultLastName)
    ,("date_of_creation",String defaultDate)
    ,("avatar",String defaultAvatar)
    ,("is_admin",Bool True)
    ,("access_key",String defaultAccessKey)]
    $ getDefaultValues ["first_name","last_name","date_of_creation","avatar","is_admin","access_key"]

build_create_EssenceValueTest =
    TestCase $
    runReaderT
        ((evalStateT
            (buildEssenceValue
                (EssenceList "author" "create"
                    [("id",MyInteger 2)
                    ,("person_id",MyInteger 1)]
                )
            )
        [])) testConfig
    >>= assertEqual
    ("for ("
    <> "runReaderT"
    <> "(evalStateT"
    <> "(buildEssenceValue testAuthorListCreate)"
    <> "[(\"author\",[(\"id\",MyInteger 2)])"
    <> ",(\"person\",[(\"id\",MyInteger 1)])]"
    <> ") testConfig)")
    (object
        ["author1" .= object
            ["id"          .= Number 2
            ,"person_id"   .= Number 1
            ,"description" .= Null
            ]
        ]
    )

build_edit_EssenceValueTest =
    TestCase $
    runReaderT
        (evalStateT
            (buildEssenceValue (EssenceList "author" "edit" []))
        []
    ) testConfig
    >>= assertEqual
    ("for "
    <> "(runReaderT"
    <> "(evalStateT"
    <> "(buildEssenceValue (EssenceList \"author\" \"edit\" []))"
    <> "[]"
    <> ") testConfig)")
    goodResultValue

build_get_EssenceValueTest =
    TestCase $
    runReaderT
        (evalStateT
            (buildEssenceValue (EssenceList "author" "get" []))
        [("author",[("id",MyInteger 2)])
        ,("person",[("id",MyInteger 1)])]
    ) testConfig
    >>= assertEqual
    ("for "
    <> "(runReaderT"
    <> "(evalStateT"
    <> "(buildEssenceValue (EssenceList \"author\" \"get\" []))"
    <> "[(\"author\",[(\"id\",MyInteger 2)])"
    <> ",(\"person\",[(\"id\",MyInteger 1)])]"
    <> ") testConfig)")
    (object
        ["author1" .= object
            ["id"          .= Number 2
            ,"person_id"      .= Number 1
            ,"description" .= Null
            ]
        ]
    )

build_delete_EssenceValueTest =
    TestCase $
    runReaderT
        (evalStateT
            (buildEssenceValue (EssenceList "author" "edit" []))
        []
    ) testConfig
    >>= assertEqual
    ("for "
    <> "(runReaderT"
    <> "(evalStateT"
    <> "(buildEssenceValue (EssenceList \"author\" \"delete\" []))"
    <> "[]"
    <> ") testConfig)")
    goodResultValue


getNeededFields_draft_Test =
    TestCase $
    assertEqual
    "for (getNeededFields \"draft\" testDraftObj)"
    [("author_id",MyInteger 2)
    ,("category_id",MyInteger 3)
    ,("content",MyString "for loxov")
    ,("date_of_creation",MyDate "2020-09-03")
    ,("name",MyString "testDraft")
    ,("id",MyInteger 1)
    ,("tag_ids",MyIntegers [4])]
    $ getNeededFields "draft" testDraftObj

getNeededFields_person_Test =
    TestCase $
    assertEqual
    "for (getNeededFields \"person\" testPersonObj)"
    [("id",MyInteger 1)]
    $ getNeededFields "person" testPersonObj

updateDataTest =
    TestCase $
    runReaderT (execStateT (updateData "person" (Object testPersonObj)) []) (Config HM.empty) >>=
    assertEqual
    "runReaderT (execStateT (updateData \"person\" testPersonObj) []) (Config HM.empty)"
    [("person",[("id",MyInteger 1)])]

chooseNameForAddingTests =
    [ TestLabel "author_chooseNameForAddingTest"  author_chooseNameForAddingTest
    , TestLabel "draft_chooseNameForAddingTest"   draft_chooseNameForAddingTest
    , TestLabel "news_chooseNameForAddingTest"    news_chooseNameForAddingTest
    , TestLabel "comment_chooseNameForAddingTest" comment_chooseNameForAddingTest
    , TestLabel "other_chooseNameForAddingTest"   other_chooseNameForAddingTest
    ]

author_chooseNameForAddingTest =
    TestCase $ assertEqual
    "for (chooseNameForAdding \"author\")" ["person"]
    $ chooseNameForAdding "author"
draft_chooseNameForAddingTest =
    TestCase $ assertEqual
    "for (chooseNameForAdding \"draft\")" ["author","category","tag"]
    $ chooseNameForAdding "draft"
news_chooseNameForAddingTest =
    TestCase $ assertEqual
    "for (chooseNameForAdding \"news\")" ["draft"]
    $ chooseNameForAdding "news"
comment_chooseNameForAddingTest =
    TestCase $ assertEqual
    "for (chooseNameForAdding \"comment\")" ["person","news"]
    $ chooseNameForAdding "comment"
other_chooseNameForAddingTest =
    TestCase $ assertEqual
    "for (chooseNameForAdding \"person\")" ["person"]
    $ chooseNameForAdding "person"

get_news_RelatedFieldsTest =
    TestCase $
    assertEqual "for (getRelatedFields \"news\" testEssenceData)"
    [("draft_id",MyInteger 1)
    ,("name",MyString "testDraft")
    ,("date_of_creation",MyDate "2020-09-03")
    ,("author_id",MyInteger 2)
    ,("category_id",MyInteger 3)
    ,("tag_ids",MyIntegers [4])
    ,("content",MyString "for loxov")
    ,("main_photo",MyEmpty)
    ,("optional_photos",MyEmpty)]
    $ getRelatedFields "news" testEssenceData
get_other_RelatedFieldsTest =
    TestCase $
    assertEqual "for (getRelatedFields \"person\" testEssenceData)"
    [("person_id",MyInteger 1)]
    $ getRelatedFields "person" testEssenceData

handleDraftCaseTest =
    TestCase $
    assertEqual "for (handleDraftCase \"draft\" [(\"tag_id\",MyInteger 1)])"
    [("tag_ids",MyIntegers [1])]
    $ handleDraftCase "draft" [("tag_id",MyInteger 1)]

testEssenceData =
    [("person",testPersonList)
    ,("draft",testDraftList)
    ]

testPersonObj = HM.singleton "person1" $
    object
        [ "id"         .= Number 1
        , "first_name" .= String "misha"
        , "last_name"  .= String "dragon"
        , "date_of_creation" .= String "2020-08-03"
        , "avatar"     .= String "uri"
        , "is_admin"   .= Bool True
        , "access_key" .= String "key"
        ]
testPersonList = [("id",MyInteger 1)]
testDraftObj = HM.singleton "draft1" $
    object
        [ "id"         .= Number 1
        , "name"       .= String "testDraft"
        , "date_of_creation" .= String "2020-09-03"
        , "author_id"  .= Number 2
        , "category_id".= Number 3
        , "tag_ids"    .= (Array . V.fromList) [Number 4]
        , "content"    .= String "for loxov"
        , "main_photo" .= Null
        , "optional_photos" .= Null
        ]
testDraftList =
    [("id",MyInteger 1)
    ,("name",MyString "testDraft")
    ,("date_of_creation",MyDate "2020-09-03")
    ,("author_id",MyInteger 2)
    ,("category_id",MyInteger 3)
    ,("tag_ids",MyIntegers [4])
    ,("content",MyString "for loxov")
    ,("main_photo",MyEmpty)
    ,("optional_photos",MyEmpty)
    ]