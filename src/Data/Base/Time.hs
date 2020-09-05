module Data.Base.Time
  ( getTime
  ) where

import qualified Data.Time.LocalTime as LocalTime

getTime :: IO String
getTime =
  LocalTime.getZonedTime >>=
  return . takeWhile (/= '.') . tail . dropWhile (/= ' ') . show
