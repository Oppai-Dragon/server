module Tests.Request.Handling
  ( requestHandlingTests
  ) where

import Config
import Data.Essence
import Data.MyValue
import Data.Request.Handling

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Tests.Request

import Test.HUnit

requestHandlingTests :: [Test]
requestHandlingTests =
  [ TestLabel "getEssenceListTest" getEssenceListTest
  , TestLabel "addAccessKeyTest" addAccessKeyTest
  , TestLabel "setEssenceListTest" setEssenceListTest
  , TestLabel "deleteAccessKeyTest" deleteAccessKeyTest
  ]

getEssenceListTest, addAccessKeyTest, setEssenceListTest, deleteAccessKeyTest ::
     Test
getEssenceListTest =
  TestCase $
  runReaderT (getEssenceList testPersonCreateReq) testConfig >>=
  assertEqual
    "for (runReaderT (getEssenceList testPersonCreateReq) testConfig)"
    (EssenceList
       "person"
       "create"
       [("last_name", MyString "dragon"), ("first_name", MyString "misha")])

addAccessKeyTest =
  TestCase $
  runReaderT
    (execStateT
       (addAccessKey testAuthorCreateReq)
       (EssenceList "author" "create" []))
    testConfig >>=
  assertEqual
    "for (runReaderT (execStateT (addAccessKey testAuthorCreateReq) (EssenceList \"author\" \"create\" [])) testConfig)"
    (EssenceList "author" "create" [("access_key", MyString "key")])

setEssenceListTest =
  TestCase $
  runReaderT (execStateT (setEssenceList testPersonCreateReq) mempty) testConfig >>=
  assertEqual
    "for (runReaderT (setEssenceList testPersonCreateReq) testConfig)"
    (EssenceList
       "person"
       "create"
       [("last_name", MyString "dragon"), ("first_name", MyString "misha")])

deleteAccessKeyTest =
  TestCase $
  runReaderT
    (execStateT
       deleteAccessKey
       (EssenceList "author" "create" [("access_key", MyString "key")]))
    testConfig >>=
  assertEqual
    "for (runReaderT (execStateT deleteAccessKey (EssenceList \"author\" \"create\" [(\"access_key\",MyString \"key\")])) testConfig)"
    (EssenceList "author" "create" [])
