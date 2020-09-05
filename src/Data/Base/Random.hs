module Data.Base.Random
  ( getRandomInteger
  ) where

import qualified System.Random as Random

getRandomInteger :: IO Integer
getRandomInteger = Random.getStdRandom (Random.randomR (1, 100000000000))
