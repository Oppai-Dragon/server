module Data.Handler
    ( Handler
    , getEssence
    , getAction
    , getQueryBS
    , getConfig
    , updateQueryBS
    , updateEssence
    , fromIO
    , ParsedRequest
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

type QueryBS     = [(ByteString, Maybe ByteString)]

type Handler a = StateT (Essence DB) (ReaderT Config IO) a

getEssence, getAction :: Handler Text
getEssence = get >>= \(hashMap,queryBS) ->
    case HM.lookup "essence" hashMap of
        Just (String text) -> pure text
        Nothing              -> pure ""
getAction = get >>= \(hashMap,queryBS) ->
    case HM.lookup "action" hashMap of
        Just (String text) -> pure text
        Nothing              -> pure ""
getQueryBS :: Handler QueryBS
getQueryBS = get >>= pure . snd
getConfig :: Handler Config
getConfig = lift ask

updateQueryBS :: QueryBS -> Handler ()
updateQueryBS newQueryBS =
    get >>= \(hashMap,oldQueryBS) ->
    put (hashMap,newQueryBS)

updateEssence :: Text -> Handler ()
updateEssence newEssence =
    let newPathInfoMap = HM.insert "essence" (String newEssence)
    in get >>= \(hashMap,queryBS) ->
    put (newPathInfoMap hashMap,queryBS)

fromIO :: IO a -> Handler a
fromIO = lift . lift