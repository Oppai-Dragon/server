module Data.Base.Parsec
  ( letterN
  , digitN
  , letterDigitN
  ) where

import Text.Parsec

letterN :: Int -> Parsec String String String
letterN 0 = return []
letterN n = letter >>= (\ch -> (ch :) <$> letterN (n - 1))

digitN :: Int -> Parsec String String String
digitN 0 = return []
digitN n = digit >>= (\ch -> (ch :) <$> digitN (n - 1))

letterDigitN :: Int -> Parsec String String String
letterDigitN 0 = return []
letterDigitN n = (letter <|> digit) >>= (\ch -> (ch :) <$> letterDigitN (n - 1))
