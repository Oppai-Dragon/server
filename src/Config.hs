module Config
  ( module Config.Set
  , module Config.Get
  , Api(..)
  , Config(..)
  , Psql(..)
  , testApi
  , testConfig
  , testPsql
  ) where

import Config.Get
import Config.Internal
import Config.Set

import System.IO.Unsafe (unsafePerformIO)

testApi :: Api
{-# NOINLINE testApi #-}
testApi = unsafePerformIO setApi

testConfig :: Config
{-# NOINLINE testConfig #-}
testConfig = unsafePerformIO setConfig

testPsql :: Psql
{-# NOINLINE testPsql #-}
testPsql = unsafePerformIO setPsql
