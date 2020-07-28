module Tests.Essence.RelationsTree.Methods
    ( essenceRelationsTreeMethodsTests
    ) where

import Config

import Data.Essence.RelationsTree
import Data.Essence.RelationsTree.Methods

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.ByteString (ByteString)

import System.IO.Unsafe (unsafePerformIO)

import Test.HUnit

essenceRelationsTreeMethodsTests =
    [ TestLabel "isEssenceRelationsTest"    isEssenceRelationsTest
    , TestLabel "unpackLeafsTest"           unpackLeafsTest
    , TestLabel "beforeUnderscoreTest"      beforeUnderscoreTest
    , TestLabel "parseObjEssenceTest"       parseObjEssenceTest
    , TestLabel "afterUnderscoreTest"       afterUnderscoreTest
    , TestLabel "getListOfPairFromObjTest"  getListOfPairFromObjTest
    , TestLabel "checkListTest"             checkListTest
    , TestLabel "getNextFieldTest"          getNextFieldTest
    , TestLabel "isRightRelationsTest"      isRightRelationsTest
    , TestLabel "getIdPairFromObjTest"      getIdPairFromObjTest
    ]

isEssenceRelationsTest =
    TestCase $
    assertEqual
    "for (isEssenceRelations \"draft\" testConfig)"
    True
    $ isEssenceRelations "draft" testConfig

unpackLeafsTest =
    TestCase $
    assertEqual
    "for (unpackLeafs \"person1\" [Leaf \"person_id\"] testObj)"
    [("person_id", "1")]
    $ unpackLeafs "person1" [Leaf "person_id"] testObj

beforeUnderscoreTest =
    TestCase $
    assertEqual "for (beforeUnderscore \"misha_lox\")"
    "misha"
    $ beforeUnderscore "misha_lox"

parseObjEssenceTest =
    TestCase $
    assertEqual "for (parseObjEssence \"person\")"
    "person1"
    $ parseObjEssence "person"

afterUnderscoreTest =
    TestCase $
    assertEqual "for (afterUnderscore \"misha_lox\")"
    "lox"
    $ afterUnderscore "misha_lox"

getListOfPairFromObjTest =
    TestCase $
    assertEqual "for (getListOfPairFromObj \"person_id\" testObj)"
    [("id",Number 1)]
    $ getListOfPairFromObj "person_id" testObj

checkListTest =
    TestCase $
    assertEqual "for (checkList \"person_id\" [(\"id\",\"1\")])"
    [("id",Number 1)]
    $ checkList "person_id" [("id","1")]

getNextFieldTest =
    TestCase $
    assertEqual
    "for (getNextField (Trunk \"kuk\" (Branch \"kek\" [])))"
    "kek"
    $ getNextField (Trunk "kuk" (Branch "kek" []))

isRightRelationsTest =
    TestCase $
    assertEqual
    "for (isRightRelations rTestObj bTestObj \"author_id\" \"draft\")"
    False
    $ isRightRelations rTestObj bTestObj "author_id" "draft"

getIdPairFromObjTest =
    TestCase $
    assertEqual
    "for (getIdPairFromObj \"draft\" bTestObj)"
    [("id", "1")]
    $ getIdPairFromObj "draft" bTestObj

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
    [("person1",
        Object $ HM.fromList
        [("id", Number 1)]
     )
    ]