module Main where

import App (app)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Autohead as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

main :: IO ()
main = Warp.run 8000 (Wai.logStdout $ Wai.autohead app)
