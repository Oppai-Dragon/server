module Config
  ( module Config.Set
  , module Config.Get
  , Api(..)
  , Config(..)
  , Psql(..)
  , UnderApp
  , SApp
  , StateT
  , WApp
  , Config.Handle(..)
  , setEng
  , Config.new
  , testHandle
  , testApi
  , testConfig
  , testPsql
  ) where

import Config.Get
import Config.Internal
import Config.Set
import Data.Essence
import Data.Monoid
import Log

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS

import System.IO.Unsafe (unsafePerformIO)

type UnderApp = ReaderT Config.Handle IO

type SApp = StateT (Essence List) UnderApp

type WApp = WriterT All UnderApp

data Handle =
  Handle
    { hConfig :: Config
    , hApi :: Api
    , hLocal :: Local
    , hLogHandle :: Log.Handle
    }
  deriving (Show, Eq)

-- For changing settings in SQL Shell (psql)
setEng :: String
setEng = "SET lc_messages = 'en_US.UTF8'; SET client_encoding = 'UTF8';"

new :: IO Config.Handle
new = do
  config <- setConfig
  api <- setApi
  loc <- setLocal
  logHandle <- Log.new
  return
    Config.Handle
      {hConfig = config, hApi = api, hLocal = loc, hLogHandle = logHandle}

testHandle :: Config.Handle
{-# NOINLINE testHandle #-}
testHandle = unsafePerformIO Config.new

testApi :: Api
{-# NOINLINE testApi #-}
testApi = unsafePerformIO setApi

testConfig :: Config
{-# NOINLINE testConfig #-}
testConfig = unsafePerformIO setConfig

testPsql :: Psql
{-# NOINLINE testPsql #-}
testPsql = unsafePerformIO setPsql
