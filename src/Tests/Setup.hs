module Tests.Setup
    ( setupTests
    ) where

import Setup

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import Control.Monad.Trans.Writer.CPS

import Test.HUnit

setupTests =
    [ TestLabel "updateEssenceFieldsTest"   updateEssenceFieldsTest
    , TestLabel "collectEssenceJsonTest"    collectEssenceJsonTest
    , TestLabel "getEssenceObjectsTest"     getEssenceObjectsTest
    , TestLabel "getAllQuerisTest"          getAllQuerisTest
    , TestLabel "getCreateQueryTest"        getCreateQueryTest
    , TestLabel "parseAllQueriesTest"       parseAllQueriesTest
    , TestLabel "iterateObjTest"            iterateObjTest
    ]

updateEssenceFieldsTest =
    TestCase $
    updateEssenceFields ["person"] testPersonObj >>=
    assertEqual "for (updateEssenceFields [\"person\"] testPersonObj)"
    undefined

collectEssenceJsonTest =
    TestCase $
    collectEssenceJson >>=
    assertEqual "for collectEssenceJson"
    undefined

getEssenceObjectsTest =
    TestCase $
    getEssenceObjects >>=
    assertEqual "for getEssenceObjects"
    undefined

getAllQuerisTest =
    TestCase $
    getAllQueris >>=
    assertEqual "for getAllQueris"
    undefined

getCreateQueryTest =
    TestCase $
    assertEqual "for (getCreateQuery testSetup)"
    ("CREATE EXTENSION IF NOT EXISTS \"pgcrypto\";"
    <> "CREATE TABLE person "
    <> "(id BIGSERIAL PRIMARY KEY"
    <> ",first_name VARCHAR(50) NOT NULL"
    <> ",last_name VARCHAR(50) NOT NULL "
    <> ",date_of_creation DATE NOT NULL"
    <> ",avatar VARCHAR(50) DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'"
    <> ",is_admin BOOLEAN DEFAULT FALSE"
    <> ",access_key UUID DEFAULT gen_random_uuid()"
    <> ");")
    $ getCreateQuery testSetupObj

parseAllQueriesTest =
    TestCase $
    assertEqual "for (parseAllQueries \"TABLE person\")"
    "CREATE TABLE person ();"
    $ parseAllQueries "TABLE person"

iterateObjTest =
    TestCase $
    assertEqual "for (iterateObj testSetup)"
    ("CREATE EXTENSION IF NOT EXISTS \"pgcrypto\""
    <> "CREATE TABLE person "
    <> "id BIGSERIAL PRIMARY KEY"
    <> ",first_name VARCHAR(50) NOT NULL"
    <> ",last_name VARCHAR(50) NOT NULL "
    <> ",date_of_creation DATE NOT NULL"
    <> ",avatar VARCHAR(50) DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'"
    <> ",is_admin BOOLEAN DEFAULT FALSE"
    <> ",access_key UUID DEFAULT gen_random_uuid()"
    <> "") $ execWriter (iterateObj testSetup)

testSetup = Object testSetupObj
testSetupObj = HM.fromList
    [("EXTENSION" , object
        [ "IF NOT EXISTS" .= String "pgcrypto"
        ]
     )
    ,("TABLE" , object
        [ "person" .= Object testPersonObj
        ]
     )
    ]

testPersonObj = HM.fromList
    [("id", (Array . V.fromList )
        [ String "BIGSERIAL"
        , String "PRIMARY KEY"
        ]
     )
    ,("first_name", (Array . V.fromList )
        [ String "VARCHAR(50)"
        , String "NOT NULL"
        ]
     )
    ,("last_name", (Array . V.fromList )
        [ String "VARCHAR(50)"
        , String "NOT NULL"
        ]
     )
    ,("date_of_creation", (Array . V.fromList )
        [ String "DATE"
        , String "NOT NULL"
        ]
     )
    ,("avatar", (Array . V.fromList )
        [ String "VARCHAR(50)"
        , String "DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'"
        ]
     )
    ,("is_admin", (Array . V.fromList )
        [ String "BOOLEAN"
        , String "DEFAULT FALSE"
        ]
     )
    ,("access_key", (Array . V.fromList )
        [ String "UUID"
        , String "DEFAULT gen_random_uuid()"
        ]
     )
    ]