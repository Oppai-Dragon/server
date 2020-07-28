module Tests.Essence.Parse
    ( essenceParseTests
    ) where

import Data.Essence.Parse

import Text.Parsec

import Test.HUnit

essenceParseTests =
    [ TestLabel "parsePsqlTest"         parsePsqlTest
    , TestLabel "parsePsqlExprTest"     parsePsqlExprTest
    ] <> parsePsqlExprTests
    <> parseTypeTests
    <> parseValueTests
    <> parseConstraintTests

parsePsqlTest =
    TestCase $
    assertEqual "for (parsePsql \"INTEGER\")"
    ("type","int")
    $ parsePsql "INTEGER"

parsePsqlExprTest =
    TestCase $
    assertEqual "for (runParser parsePsqlExpr \"\" \"\" \"INTEGER\")"
    (Right ("type","int"))
    $ runParser parsePsqlExpr "" "" "INTEGER"

parsePsqlExprTests =
    [ TestLabel "parseTypeTest"         parseTypeTest
    , TestLabel "parseValueTest"        parseValueTest
    , TestLabel "parseRelationsTest"    parseRelationsTest
    , TestLabel "parseConstraintTest"   parseConstraintTest
    ]

parseTypeTest =
    TestCase $
    assertEqual "for (runParser parseType \"\" \"\" \"DATE\")"
    (Right ("type","date"))
    $ runParser parseType "" "" "DATE"
parseValueTest =
    TestCase $
    assertEqual "for (runParser parseValue \"\" \"\" \"NOT NULL\")"
    (Right ("value","not null"))
    $ runParser parseValue "" "" "NOT NULL"
parseRelationsTest =
    TestCase $
    assertEqual "for (runParser parseRelations \"\" \"\" \"REFERENCES person (id)\")"
    (Right ("relations","with person by id"))
    $ runParser parseRelations "" "" "REFERENCES person (id)"
parseConstraintTest =
    TestCase $
    assertEqual "for (runParser parseConstraint \"\" \"\" \"PRIMARY KEY\")"
    (Right ("constraint","primary key"))
    $ runParser parseConstraint "" "" "PRIMARY KEY"

parseTypeTests =
    [ TestLabel "parseDateTest"         parseDateTest
    , TestLabel "parseBoolTest"         parseBoolTest
    , TestLabel "parseUuidTest"         parseUuidTest
    , TestLabel "parseIntTest"          parseIntTest
    , TestLabel "parseIntArrTest"       parseIntArrTest
    , TestLabel "parseStrTest"          parseStrTest
    , TestLabel "parseStrArrTest"       parseStrArrTest
    ]

parseDateTest =
    TestCase $
    assertEqual "for (runParser parseDate \"\" \"\" )"
    (Right "date")
    $ runParser parseDate "" "" "DATE"
parseBoolTest =
    TestCase $
    assertEqual "for (runParser parseBool \"\" \"\" \"BOOLEAN\")"
    (Right "bool")
    $ runParser parseBool "" "" "BOOLEAN"
parseUuidTest =
    TestCase $
    assertEqual "for (runParser parseUuid \"\" \"\" \"UUID\")"
    (Right "uuid")
    $ runParser parseUuid "" "" "UUID"
parseIntTest =
    TestCase $
    assertEqual "for (runParser parseInt \"\" \"\" \"INTEGER\")"
    (Right "int")
    $ runParser parseInt "" "" "INTEGER"
parseIntArrTest =
    TestCase $
    assertEqual "for (runParser parseIntArr \"\" \"\" \"INTEGER[]\")"
    (Right "array int")
    $ runParser parseIntArr "" "" "INTEGER[]"
parseStrTest =
    TestCase $
    assertEqual "for (runParser parseStr \"\" \"\" \"VARCHAR(12)\")"
    (Right "string")
    $ runParser parseStr "" "" "VARCHAR(12)"
parseStrArrTest =
    TestCase $
    assertEqual "for (runParser parseStrArr \"\" \"\" \"VARCHAR(12)[]\")"
    (Right "array string")
    $ runParser parseStrArr "" "" "VARCHAR(12)[]"

parseValueTests =
    [ TestLabel "parseNotNullTest"      parseNotNullTest
    , TestLabel "parseDefaultTest"      parseDefaultTest
    ]

parseNotNullTest =
    TestCase $
    assertEqual "for (runParser parseNotNull \"\" \"\" \"NOT NULL\")"
    (Right "not null")
    $ runParser parseNotNull "" "" "NOT NULL"
parseDefaultTest =
    TestCase $
    assertEqual "for (runParser parseDefault \"\" \"\" \"DEFAULT\")"
    (Right "null")
    $ runParser parseDefault "" "" "DEFAULT"

parseConstraintTests =
    [ TestLabel "parsePrimaryKeyTest"   parsePrimaryKeyTest
    , TestLabel "parseUniqueTest"       parseUniqueTest
    , TestLabel "parseOnActionTest"     parseOnActionTest
    ]

parsePrimaryKeyTest =
    TestCase $
    assertEqual "for (runParser parsePrimaryKey \"\" \"\" \"PRIMARY KEY\")"
    (Right "primary key")
    $ runParser parsePrimaryKey "" "" "PRIMARY KEY"
parseUniqueTest =
    TestCase $
    assertEqual "for (runParser parseUnique \"\" \"\" \"UNIQUE\")"
    (Right "unique")
    $ runParser parseUnique "" "" "UNIQUE"
parseOnActionTest =
    TestCase $
    assertEqual "for (runParser parseOnAction \"\" \"\" \"ON DELETE CASCADE\")"
    (Right "delete with parent")
    $ runParser parseOnAction "" "" "ON DELETE CASCADE"