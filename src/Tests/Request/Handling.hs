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
  runReaderT (getEssenceList testPersonCreateReq) testHandle >>=
  assertEqual
    "for (runReaderT (getEssenceList testPersonCreateReq) testHandle)"
    EssenceList
      { elName = "person"
      , elAction = "create"
      , elList =
          [("last_name", MyString "dragon"), ("first_name", MyString "misha")]
      }

addAccessKeyTest =
  TestCase $
  runReaderT
    (execStateT
       (addAccessKey testAuthorCreateReq)
       mempty {elName = "author", elAction = "create"})
    testHandle >>=
  assertEqual
    "for (runReaderT (execStateT (addAccessKey testAuthorCreateReq) (EssenceList \"author\" \"create\" [])) testHandle)"
    EssenceList
      { elName = "author"
      , elAction = "create"
      , elList = [("access_key", MyString "key")]
      }

setEssenceListTest =
  TestCase $
  runReaderT (execStateT (setEssenceList testPersonCreateReq) mempty) testHandle >>=
  assertEqual
    "for (runReaderT (setEssenceList testPersonCreateReq) testHandle)"
    EssenceList
      { elName = "person"
      , elAction = "create"
      , elList =
          [("last_name", MyString "dragon"), ("first_name", MyString "misha")]
      }

deleteAccessKeyTest =
  TestCase $
  runReaderT
    (execStateT
       deleteAccessKey
       EssenceList
         { elName = "author"
         , elAction = "create"
         , elList = [("access_key", MyString "key")]
         })
    testHandle >>=
  assertEqual
    "for (runReaderT (execStateT deleteAccessKey (EssenceList \"author\" \"create\" [(\"access_key\",MyString \"key\")])) testHandle)"
    mempty {elName = "author", elAction = "create"}
