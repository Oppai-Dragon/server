module Tests.Essence.Parse.Clause
  ( essenceParseClauseTests
  ) where

import Data.Essence
import Data.Essence.Parse.Clause
import Data.MyValue
import Data.SQL

import Control.Monad.Trans.Writer.CPS

import Tests.Essence

import Test.HUnit

essenceParseClauseTests :: [Test]
essenceParseClauseTests =
  [ TestLabel "toEssenceClauseTest" toEssenceClauseTest
  , TestLabel "pickTableNameTest" pickTableNameTest
  , TestLabel "pickClauseTest" pickClauseTest
  , TestLabel "matchEssenceTest" matchEssenceTest
  , TestLabel "addEssenceNameTest" addEssenceNameTest
  , TestLabel "parseTagsInTest" parseTagsInTest
  , TestLabel "parseAuthorNameTest" parseAuthorNameTest
  , TestLabel "parseFilterAuthorAnyNameTest" parseFilterAuthorAnyNameTest
  , TestLabel "parseSearchAuthorAnyNameTest" parseSearchAuthorAnyNameTest
  , TestLabel "parseFilterAuthorFullNameTest" parseFilterAuthorFullNameTest
  , TestLabel "parseSearchAuthorFullNameTest" parseSearchAuthorFullNameTest
  , TestLabel "parseFullNameTest" parseFullNameTest
  , TestLabel "parseSearchStrTest" parseSearchStrTest
  , TestLabel "parseStrTest" parseStrTest
  , TestLabel "parseSubStrTest" parseSubStrTest
  ]

toEssenceClauseTest, pickTableNameTest, pickClauseTest, matchEssenceTest, addEssenceNameTest, parseTagsInTest, parseAuthorNameTest, parseFilterAuthorAnyNameTest, parseSearchAuthorAnyNameTest, parseFilterAuthorFullNameTest, parseSearchAuthorFullNameTest, parseFullNameTest, parseSearchStrTest, parseStrTest, parseSubStrTest ::
     Test
toEssenceClauseTest =
  TestCase $
  assertEqual
    "for (toEssenceClause )"
    EssenceClause
      { ecNameList = ["author", "person", "category"]
      , ecClauseList =
          [ Where ("news.id", MyInteger 1)
          , Filter $
            "(person.first_name ILIKE 'misha' AND person.last_name ILIKE 'dragon') " <>
            "OR (person.first_name ILIKE 'dragon' AND person.last_name ILIKE 'misha')"
          , Filter "category.name ILIKE '%cat%'"
          , OrderBy "news.date_of_creation"
          ]
      } $
  execWriter $
  toEssenceClause
    EssenceList
      {elName = "news", elAction = "create", elList = testNewsGetFields}

pickTableNameTest =
  TestCase $
  assertEqual "for (pickTableName \"filter_author_name\")" ["author", "person"] $
  pickTableName "filter_author_name"

pickClauseTest =
  TestCase $
  assertEqual
    "for (pickClause (\"filter_author_name\",MyString \"misha dragon\"))"
    [ Filter $
      "(person.first_name ILIKE 'misha' AND person.last_name ILIKE 'dragon') " <>
      "OR (person.first_name ILIKE 'dragon' AND person.last_name ILIKE 'misha')"
    ] $
  pickClause "news" ("filter_author_name", MyString "misha dragon")

matchEssenceTest =
  TestCase $
  assertEqual
    "for (matchEssence [\"news\",\"author\",\"person\",\"category\",\"tag\"])"
    [ Filter "news.author_id=author.id"
    , Filter "author.person_id=person.id"
    , Filter "news.category_id=category.id"
    , Filter "tag.id=ANY(news.tag_ids)"
    ] $
  matchEssence ["author", "person", "category", "tag"]

addEssenceNameTest =
  TestCase $
  assertEqual
    "for (addEssenceName \"person\" \"date_of_creation\")"
    "person.date_of_creation" $
  addEssenceName "person" "date_of_creation"

parseTagsInTest =
  TestCase $
  assertEqual
    "for (parseTagsIn $ MyIntegerArr [1,2,3])"
    "1=ANY(tag_ids) OR 2=ANY(tag_ids) OR 3=ANY(tag_ids)" $
  parseTagsIn $ MyIntegerArr [1, 2, 3]

parseAuthorNameTest =
  TestCase $
  assertEqual
    "for (parseAuthorName \"filter\" \"misha dragon\")"
    ("(person.first_name ILIKE 'misha' AND person.last_name ILIKE 'dragon'" <>
     ") OR (" <>
     "person.first_name ILIKE 'dragon' AND person.last_name ILIKE 'misha')") $
  parseAuthorName "filter" "misha dragon"

parseFilterAuthorAnyNameTest =
  TestCase $
  assertEqual
    "for (parseAuthorAnyName \"search\" \"dragon\")"
    "person.first_name ILIKE 'dran' OR person.last_name ILIKE 'dran'" $
  parseAuthorAnyName "filter" "dran"

parseSearchAuthorAnyNameTest =
  TestCase $
  assertEqual
    "for (parseAuthorAnyName \"search\" \"dragon\")"
    "person.first_name ILIKE '%dran%' OR person.last_name ILIKE '%dran%'" $
  parseAuthorAnyName "search" "dran"

parseFilterAuthorFullNameTest =
  TestCase $
  assertEqual
    "for (parseAuthorFullName \"sort\" (\"misha\",\"dragon\"))"
    ("(person.first_name ILIKE 'misha' AND person.last_name ILIKE 'dragon'" <>
     ") OR (" <>
     "person.first_name ILIKE 'dragon' AND person.last_name ILIKE 'misha')") $
  parseAuthorFullName "filter" ("misha", "dragon")

parseSearchAuthorFullNameTest =
  TestCase $
  assertEqual
    "for (parseAuthorFullName \"sort\" (\"misha\",\"dragon\"))"
    ("(person.first_name ILIKE '%misha%' AND person.last_name ILIKE '%dragon%'" <>
     ") OR (" <>
     "person.first_name ILIKE '%dragon%' AND person.last_name ILIKE '%misha%')") $
  parseAuthorFullName "search" ("misha", "dragon")

parseFullNameTest =
  TestCase $
  assertEqual "for (parseFullName \"misha dragon\")" ("misha", "dragon") $
  parseFullName "misha dragon"

parseSearchStrTest =
  TestCase $
  assertEqual "for (parseSearchStr \"is\")" "%is%" $ parseSearchStr "is"

parseStrTest =
  TestCase $ assertEqual "for (parseStr \"misha\")" "'misha'" $ parseStr "misha"

parseSubStrTest =
  TestCase $ assertEqual "for (parseSubStr \"mis\")" "mis%" $ parseSubStr "mis"
