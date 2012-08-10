module Main where

import Control.Monad (void)
import Control.Concurrent

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.Wai.Dispatch
import Application
import Database
import Routes

forkIO_ f = void $ forkIO (void f)

main = do
	putStrLn "Running..."
	chan <- newChan
	void $ forkIO_ (database chan)
	run 3000 (logStdoutDev $ dispatch on404 $ routes chan)
