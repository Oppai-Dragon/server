module Tests.Database.Create
    ( databaseCreateTests
    ) where

import Config
import Database.Create
import Data.Essence
import Data.MyValue

import Data.Time.Clock

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict

import Tests.Essence

import Test.HUnit

databaseCreateTests =
    [ TestLabel "addingDefaultTest"     addingDefaultTest
    , TestLabel "addIdTest"             addIdTest
    ]

addingDefaultTest = TestCase $ do
    date <- getCurrentTime
    let dateValue = show $ utctDay date
    result <- runReaderT (execStateT addingDefault testPersonListCreate) testConfig
    assertEqual "runReaderT (execStateT addingDefault testPersonListCreate) testConfig"
        (EssenceList "person" "create"
            [("first_name",MyString "testFirstName")
            ,("last_name",MyString "testLastName")
            ,("avatar",MyString "uri")
            ,("access_key",MyString accessKeyStr)
            ,("is_admin",MyBool True)
            ,("id",MyNextval "nextval('person_id_seq')")
            ]
        ) result

addIdTest =
    TestCase $
    runReaderT (execStateT addId testPersonListCreate) testConfig >>=
    assertEqual "runReaderT (execStateT addId testPersonListCreate) testConfig"
    (EssenceList "person" "create"
        [("first_name",MyString "testFirstName")
        ,("last_name",MyString "testLastName")
        ,("avatar",MyString "uri")
        ,("access_key",MyString accessKeyStr)
        ,("is_admin",MyBool True)
        ,("id",MyNextval "nextval('person_id_seq')")
        ]
    )