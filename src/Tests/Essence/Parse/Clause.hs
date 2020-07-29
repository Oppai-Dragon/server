module Tests.Essence.Parse.Clause
    ( essenceClauseTests
    ) where

import Data.Essence.Parse.Clause

import Test.HUnit

essenceClauseTests =
    [ TestLabel "parseTagsInTest"           parseTagsInTest
    , TestLabel "parseAuthorNameTest"       parseAuthorNameTest
    , TestLabel "parseAuthorAnyNameTest"    parseAuthorAnyNameTest
    , TestLabel "parseAuthorFullNameTest"   parseAuthorFullNameTest
    , TestLabel "parseFullNameTest"         parseFullNameTest
    , TestLabel "parseSearchStrTest"        parseSearchStrTest
    , TestLabel "parseStrTest"              parseStrTest
    , TestLabel "parseSubStrTest"           parseSubStrTest
    ]

parseTagsInTest =
    TestCase $
    assertEqual "for (parseTagsIn \"[1,2,3]\")"
    "1=ANY(tag_ids) OR 2=ANY(tag_ids) OR 3=ANY(tag_ids)"
    $ parseTagsIn "[1,2,3]"

parseAuthorNameTest =
    TestCase $
    assertEqual "for (parseAuthorName \"filter\" \"misha dragon\")"
    ("(first_name ILIKE('misha') AND last_name ILIKE('dragon')"
    <> ") OR ("
    <> "first_name ILIKE('dragon') AND last_name ILIKE('misha'))")
    $ parseAuthorName "filter" "misha dragon"

parseAuthorAnyNameTest =
    TestCase $
    assertEqual "for (parseAuthorAnyName \"search\" \"dragon\")"
    "first_name ILIKE('%dran%') OR last_name ILIKE('%dran%')"
    $ parseAuthorAnyName "search" "dran"

parseAuthorFullNameTest =
    TestCase $
    assertEqual "for (parseAuthorFullName \"sort\" (\"misha\",\"dragon\"))"
    ("(first_name ILIKE('misha') AND last_name ILIKE('dragon')"
    <> ") OR ("
    <> "first_name ILIKE('dragon') AND last_name ILIKE('misha'))")
    $ parseAuthorFullName "filter" ("misha","dragon")

parseFullNameTest =
    TestCase $
    assertEqual "for (parseFullName \"misha dragon\")"
    ("misha","dragon")
    $ parseFullName "misha dragon"

parseSearchStrTest =
    TestCase $
    assertEqual "for (parseSearchStr \"is\")"
    "'%is%'"
    $ parseSearchStr "is"

parseStrTest =
    TestCase $
    assertEqual "for (parseStr \"misha\")"
    "'misha'"
    $ parseStr "misha"

parseSubStrTest =
    TestCase $
    assertEqual "for (parseSubStr \"mis\")"
    "'mis%'"
    $ parseSubStr "mis"
