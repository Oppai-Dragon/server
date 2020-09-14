module Database.Update
  ( updateDatabase
  ) where

import Config
import Data.Base
import Log
import Setup

--import Control.Monad
import Data.List
import qualified Data.Text as T

type EssenceLocal = [String]

type EssenceDatabase = [String]

type EssenceApi = [String]

--type EssenceNew = [String]
updateDatabase :: IO ()
updateDatabase = do
  essences <- map T.unpack . getEssences <$> setApi
  essenceDatabaseArr <- getEssenceArr "EssenceDatabase"
  essenceLocalArr <- getEssenceArr "EssenceLocal"
  if isRedundantTables essences essenceDatabaseArr essenceLocalArr
    then dropTables $ essenceDatabaseArr \\ essenceLocalArr
    else traceIO "All tables are needed"
  if isNewTables essences essenceDatabaseArr essenceLocalArr
    then createTables $ essenceLocalArr \\ essenceDatabaseArr
    else traceIO "No need add tables"
  --when isTablesChanged updateTables

isRedundantTables, isNewTables :: EssenceApi -> EssenceDatabase -> EssenceLocal -> Bool
isRedundantTables apiArr dbArr locArr =
  null (apiArr \\ locArr) && (not . null) (dbArr \\ locArr)
isNewTables apiArr dbArr locArr =
  null (apiArr \\ locArr) && (not . null) (locArr \\ dbArr)
--isTablesChanged :: Bool
--isTablesChanged = undefined
--updateTables :: IO ()
--updateTables = undefined
