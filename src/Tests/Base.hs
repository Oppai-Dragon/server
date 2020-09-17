module Tests.Base
  ( baseTests
  ) where

import Tests.Base.Aeson
import Tests.Base.Path
import Tests.Base.Prelude
import Tests.Base.Text

import Test.HUnit

baseTests :: [Test]
baseTests = baseAesonTests <> basePathTests <> basePreludeTests <> baseTextTests
