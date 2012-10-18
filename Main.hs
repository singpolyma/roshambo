module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import Control.Monad (void)
import Control.Concurrent
import Network.URI (parseAbsoluteURI, URI(..))

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)

import Network.Wai.Dispatch
import Application
import Database
import Routes

forkIO_ :: IO a -> IO ()
forkIO_ f = void $ forkIO (void f)

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

main :: IO ()
main = main' . map (fmap addTrailingSlash . parseAbsoluteURI) =<< getArgs
	where
	main' [Just root@(URI {uriAuthority = Just _})] = do
		putStrLn "Running..."
		chan <- newChan
		forkIO_ (database chan)
		run 3000 $
			logStdoutDev $ autohead $ acceptOverride $ jsonp $ -- Middleware
			dispatch on404 $ routes root chan                  -- Do routing
	main' _ = hPutStrLn stderr "Usage: ./Main <Root URI>"
