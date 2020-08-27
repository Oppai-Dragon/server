module Tests.Database.Create
  ( databaseCreateTests
  ) where

import Config
import Data.Essence
import Data.MyValue
import Database.Create

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Tests.Essence

import Test.HUnit

databaseCreateTests :: [Test]
databaseCreateTests =
  [ TestLabel "addingDefaultTest" addingDefaultTest
  , TestLabel "addIdTest" addIdTest
  ]

addingDefaultTest, addIdTest :: Test
addingDefaultTest =
  TestCase $ do
    result <-
      runReaderT (execStateT addingDefault testPersonListCreate) testConfig
    assertEqual
      "runReaderT (execStateT addingDefault testPersonListCreate) testConfig"
      (EssenceList
         "person"
         "create"
         [ ("first_name", MyString testFirstName)
         , ("last_name", MyString testLastName)
         , ("avatar", MyString testAvatar)
         , ("access_key", MyString testAccessKey)
         , ("is_admin", MyBool True)
         , ("id", MyNextval "nextval('person_id_seq')")
         ])
      result

addIdTest =
  TestCase $
  runReaderT (execStateT addId testPersonListCreate) testConfig >>=
  assertEqual
    "runReaderT (execStateT addId testPersonListCreate) testConfig"
    (EssenceList
       "person"
       "create"
       [ ("first_name", MyString testFirstName)
       , ("last_name", MyString testLastName)
       , ("avatar", MyString testAvatar)
       , ("access_key", MyString testAccessKey)
       , ("is_admin", MyBool True)
       , ("id", MyNextval "nextval('person_id_seq')")
       ])
