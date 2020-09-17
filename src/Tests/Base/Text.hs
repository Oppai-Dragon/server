module Tests.Base.Text
  ( baseTextTests
  ) where

import Data.Base.Text

import Test.HUnit

baseTextTests :: [Test]
baseTextTests = [TestLabel "findTextTest" findTextTest]

findTextTest :: Test
findTextTest =
  TestCase $
  assertEqual "for (findText \"k1\" [\"k2\",\"k1\"])" (Just "k1") $
  findText "k1" ["k2", "k1"]