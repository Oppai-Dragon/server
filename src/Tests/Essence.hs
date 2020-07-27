module Tests.Essence where

import Data.MyValue
import Data.Essence

import qualified Data.HashMap.Strict as HM

testEssenceDB = EssenceDB "person" "create" $ HM.fromList testEssenceDBFields

testEssenceDBFields =
    [("id", Description (MyNextval "") (Just $ NOT NULL) Nothing Nothing)
    ,("first_name", Description (MyString "") (Just $ NOT NULL) Nothing Nothing)
    ,("last_name", Description (MyString "") (Just $ NOT NULL) Nothing Nothing)
    ,("date_of_creation", Description (MyString "") (Just $ NOT NULL) Nothing Nothing)
    ,("avatar", Description (MyString "") (Just NULL) Nothing Nothing)
    ,("is_admin", Description (MyBool False) (Just NULL) Nothing Nothing)
    ,("access_key", Description (MyString "") (Just NULL) Nothing Nothing)
    ]