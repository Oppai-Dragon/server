module Data.Handler
    (
    ) where

import Config
import Data.Essence

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict                    as HM
import Data.Text (Text)

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class          (lift)

type QueryMBS     = [(ByteString, Maybe ByteString)]
