module Data.Base.Time
  ( getTime
  ) where

import qualified Data.Time.LocalTime as LocalTime

getTime :: IO String
getTime =
  takeWhile (/= '.') . tail . dropWhile (/= ' ') . show <$>
  LocalTime.getZonedTime
