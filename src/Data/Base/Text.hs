module Data.Base.Text
  ( findText
  ) where

import qualified Data.Text as T

findText :: T.Text -> [T.Text] -> Maybe T.Text
findText _ [] = Nothing
findText text (textX:rest) =
  if text == textX
    then Just textX
    else findText text rest
