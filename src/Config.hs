module Config
  ( module Config.Set
  , module Config.Get
  , Api(..)
  , Config(..)
  , Psql(..)
  , UnderApp
  , SApp
  , S
  , WApp
  , W
  , Config.Handle(..)
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

type S a b = StateT a b

type WApp = WriterT All UnderApp

type W a b = Writer a b

data Handle =
  Handle
    { hConfig :: Config
    , hLocal :: Local
    , hLog :: Log.Handle
    }
  deriving (Show, Eq)

new :: IO Config.Handle
new = do
  config <- setConfig
  loc <- setLocal
  Config.Handle config loc <$> Log.new

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
