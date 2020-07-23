module Data.Request.Access.Methods.Test
    ( requestAccessMethodsTests
    ) where

import Config

import Data.Request.Access
import Data.Request.Access.Methods

import Data.Text (Text)

import Test.HUnit

requestAccessMethodsTests =
    [ TestLabel "groupActionsAccessTest" groupActionsAccessTest
    , TestLabel "isFindAccessTest" isFindAccessTest
    , TestLabel "findActionTest" findActionTest
    , TestLabel "iterateActionsTest" iterateActionsTest
    , TestLabel "isFindEssenceTest" isFindEssenceTest
    , TestLabel "isAccessTest" isAccessTest
    ]

groupActionsAccessTest =
    TestCase $
    assertEqual
    ("for (groupActionsAccess "
    <> "[(\"create\", Admin)"
    <> ",(\"edit\", Admin)"
    <> ",(\"get\", Everyone)"
    <> ",(\"delete\", Admin)"
    <> "]"
    <> ")")
    [(["get"],Everyone)
    ,(["create","edit","delete"], Admin)
    ]
    $ groupActionsAccess
    [("create", Admin)
    ,("edit", Admin)
    ,("get", Everyone)
    ,("delete", Admin)
    ]

isFindAccessTest =
    TestCase $
    assertEqual "for (isFindAccess Admin [Admin])"
    True
    $ isFindAccess Admin [Admin]

findActionTest =
    TestCase $
    assertEqual
    ("for (findAction "
    <> "\"create\" "
    <> "[([\"get\",Everyone])"
    <> ",([\"create\",\"edit\",\"delete\"], Admin)"
    <> "]"
    <> ")")
    (Just Admin)
    $ findAction "create"
    [(["get"],Everyone)
    ,(["create","edit","delete"], Admin)
    ]

iterateActionsTest =
    TestCase $
    assertEqual
    ("for (iterateActions "
    <> "\"category\" \"create\" "
    <> "[Admin] testConfig"
    <> ")")
    True
    $ iterateActions "category" "create" [Admin] testConfig

isFindEssenceTest =
    TestCase $
    assertEqual
    ("for (isFindEssence "
    <> "\"category\" [\"category\"]"
    <> ")")
    True
    $ isFindEssence "category" ["category"]

isAccessTest =
    TestCase $
    assertEqual
    ("for (isAccess "
    <> "\"category\" \"create\" "
    <> "[Admin] testConfig"
    <> ")")
    True
    $ isAccess "category" "create" [Admin] testConfig
