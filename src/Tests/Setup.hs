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
    [ TestLabel "getCreateQueryTest"        getCreateQueryTest
    , TestLabel "parseAllQueriesTest"       parseAllQueriesTest
    , TestLabel "iterateObjTest"            iterateObjTest
    ]

getCreateQueryTest =
    TestCase $
    assertEqual "for (getCreateQuery testSetup)"
    ("TABLE person "
    <> "( first_name VARCHAR(50) NOT NULL"
    <> " ,access_key UUID DEFAULT gen_random_uuid()"
    <> " ,date_of_creation DATE NOT NULL"
    <> " ,last_name VARCHAR(50) NOT NULL"
    <> " ,is_admin BOOLEAN DEFAULT FALSE"
    <> " ,id BIGSERIAL PRIMARY KEY"
    <> " ,avatar VARCHAR(50) DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'"
    <> " );")
    $ getCreateQuery testSetupObj

parseAllQueriesTest =
    TestCase $
    assertEqual "for (parseAllQueries \"TABLE person\")"
    "TABLE person ( id INT );"
    $ parseAllQueries "TABLE person id INT ,"

iterateObjTest =
    TestCase $
    assertEqual "for (iterateObj testSetup)"
    ("TABLE person "
    <> "first_name VARCHAR(50) NOT NULL"
    <> " ,access_key UUID DEFAULT gen_random_uuid()"
    <> " ,date_of_creation DATE NOT NULL"
    <> " ,last_name VARCHAR(50) NOT NULL"
    <> " ,is_admin BOOLEAN DEFAULT FALSE"
    <> " ,id BIGSERIAL PRIMARY KEY"
    <> " ,avatar VARCHAR(50) DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'"
    <> " ,") $ execWriter (iterateObj testSetup)

testSetup = Object testSetupObj
testSetupObj = HM.fromList
    [("TABLE" , object
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