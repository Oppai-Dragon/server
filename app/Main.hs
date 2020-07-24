module Main where


import App

import           Network.Wai
import qualified Network.Wai.Handler.Warp               as Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Autohead



main :: IO ()
main = Warp.run 8000 $ logStdout $ autohead app
