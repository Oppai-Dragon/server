module Data.Essence.Relations.Methods.Test
    ( essenceRelationsMethodsTests
    ) where

import Config

import Data.Essence.Relations
import Data.Essence.Relations.Methods

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.ByteString (ByteString)

import System.IO.Unsafe (unsafePerformIO)

import Test.HUnit

essenceRelationsMethodsTests =
    [ TestLabel "isEssenceRelationsTest"    isEssenceRelationsTest
    , TestLabel "unpackLeafsTest"           unpackLeafsTest
    , TestLabel "beforeUnderscoreTest"      beforeUnderscoreTest
    , TestLabel "parseObjEssenceTest"       parseObjEssenceTest
    , TestLabel "afterUnderscoreTest"       afterUnderscoreTest
    , TestLabel "getListOfPairFromObjTest"  getListOfPairFromObjTest
    , TestLabel "checkListTest"             checkListTest
    , TestLabel "getNextFieldTest"          getNextFieldTest
    , TestLabel "isRightRelationsTest"      isRightRelationsTest
    , TestLabel "getIdFromQueryBSTest"      getIdFromQueryBSTest
    ]

isEssenceRelationsTest =
    TestCase $
    assertEqual
    "for (isEssenceRelations \"draft\" testConfig)"
    True
    $ isEssenceRelations "draft" testConfig

--getRelationsFieldsTest =
--    TestCase $
--    assertEqual
--    "for (getRelationsFields \"draft\" testConfig)"
--    ([()])
--    $ unsafePerformIO
--    $ getRelationsFields "draft"

beforeUnderscoreTest =
    TestCase $
    assertEqual "for (beforeUnderscore \"misha_lox\")"
    "misha"
    $ beforeUnderscore "misha_lox"

parseObjEssenceTest =
    TestCase $
    assertEqual "for (parseObjEssence \"user\")"
    "users1"
    $ parseObjEssence "user"

afterUnderscoreTest =
    TestCase $
    assertEqual "for (afterUnderscore \"misha_lox\")"
    "lox"
    $ afterUnderscore "misha_lox"

--findEssenceTest =
--    TestCase $
--    assertEqual "for (findEssence )"
--    ()
--    $ findEssence

getQueryBSFromObjTest =
    TestCase $
    assertEqual
    "for (getQueryBSFromObj \"draft_id\" bTestObj)"
    [("draft_id", Just "1")]
    $ getQueryBSFromObj "draft_id" bTestObj

checkQueryBSTest =
    TestCase $
    assertEqual
    "for (checkQueryBS \"draft_id\" [(\"id\", Just \"1\")])"
    [("id", Just "1")]
    $ checkQueryBS "draft_id" [("id", Just "1")]

unpackLeafsTest =
    TestCase $
    assertEqual
    "for (unpackLeafs \"users1\" [Leaf \"user_id\"] testObj)"
    [("user_id", Number 1)]
    $ unpackLeafs "users1" [Leaf "user_id"] testObj

getNextFieldTest =
    TestCase $
    assertEqual
    "for (getNextField (Trunk \"kuk\" (Branch \"kek\" [])))"
    "kek"
    $ getNextField (Trunk "kuk" (Branch "kek" []))

--relationsHandlerTest =
--    TestCase $
--    assertEqual "for (relationsHandler )"
--    ()
--    $ relationsHandler "draft" [()] testConfig

isRightRelationsTest =
    TestCase $
    assertEqual
    "for (isRightRelations rTestObj bTestObj \"author_id\" \"draft\")"
    False
    $ isRightRelations rTestObj bTestObj "author_id" "draft"

getIdFromQueryBSTest =
    TestCase $
    assertEqual
    "for (getIdFromQueryBS \"draft\" bTestObj)"
    [("id", Just "1")]
    $ getIdFromQueryBS "draft" bTestObj

rTestObj = HM.fromList
    [("author1",
        Object $ HM.fromList
        [("id", Number 1)
        ]
     )
    ]
bTestObj = HM.fromList
    [("draft1",
        Object $ HM.fromList
        [("author_id", Number 2)
        ,("id", Number 1)
        ]
     )
    ]

testObj = HM.fromList
    [("users1",
        Object $ HM.fromList
        [("id", Number 1)]
     )
    ]