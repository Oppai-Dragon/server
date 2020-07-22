module Setup.Test
    ( setupTests
    ) where

import Setup

import Test.HUnit

setupTests =
    [ TestLabel "getCreateQueriesTest" getCreateQueriesTest
    ]

getCreateQueriesTest =
    TestCase $
    assertEqual "for (getCreateQueries testSetup)"
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
    $ getCreateQueries testSetup


testSetup = object
    [ "CREATE" .= object
        [ "EXTENSION" .= object
            [ "IF NOT EXISTS" .= String "pgcrypto"
            ]
        , "TABLE" .= object
            [ "person" .= object
                [ "id" .= (Array . V.fromList )
                    [ "BIGSERIAL"
                    , "PRIMARY KEY"
                    ]
                , "first_name" .= (Array . V.fromList )
                    [ "VARCHAR(50)"
                    , "NOT NULL"
                    ]
                , "last_name" .= (Array . V.fromList )
                    [ "VARCHAR(50)"
                    , "NOT NULL"
                    ]
                , "date_of_creation" .= (Array . V.fromList )
                    [ "DATE"
                    , "NOT NULL"
                    ]
                , "avatar" .= (Array . V.fromList )
                    [ "VARCHAR(50)"
                    , "DEFAULT 'https://oppai-dragon.site/images/avatar.jpg'"
                    ]
                , "is_admin" .= (Array . V.fromList )
                    [ "BOOLEAN"
                    , "DEFAULT FALSE"
                    ]
                , "access_key" .= (Array . V.fromList )
                    [ "UUID"
                    , "DEFAULT gen_random_uuid()"
                    ]
                ]
            ]
        ]
    ]