module Tests.Base.Path
  ( basePathTests
  ) where

import Data.Base.Path

import Test.HUnit

basePathTests :: [Test]
basePathTests = [TestLabel "parsePathTest" parsePathTest]

parsePathTest :: Test
parsePathTest =
  TestCase $
  assertEqual "for (parsePath \"C:Repo/server/src\")" "C:Repo/server" $
  parsePath "C:Repo/server/src"
