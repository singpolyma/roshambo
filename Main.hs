module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import Control.Monad (void)
import Control.Concurrent
import Network.URI (parseAbsoluteURI, URI(..))

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)

import Control.Monad.Trans.Resource (transResourceT)
import EitherIO

import Network.Wai.Dispatch
import Application
import Database
import Routes

forkIO_ :: IO a -> IO ()
forkIO_ f = void $ forkIO (void f)

mapRoute :: (a -> b) -> Route a -> Route b
mapRoute mapApp route = route {
		rhDispatch = (\x -> fmap mapApp (rhDispatch route x))
	}

applicationEitherIO :: (IOError -> Request -> IO Response) -> ApplicationEitherIO -> Application
applicationEitherIO errorPage app x = transResourceT (handleEitherIO (`errorPage` x)) (app x)

main :: IO ()
main = main' . map parseAbsoluteURI =<< getArgs
	where
	handleApp = applicationEitherIO $ flip $ const $ errorPage . show
	main' [Just root@(URI {uriAuthority = Just _})] = do
		putStrLn "Running..."
		chan <- newChan
		forkIO_ (database chan)
		run 3000 $
			logStdoutDev $ autohead $ acceptOverride $ jsonp $ -- Middleware
			dispatch (handleApp on404) $                       -- Do routing
			map (mapRoute handleApp) $ routes root chan        -- On these routes
	main' _ = hPutStrLn stderr "Usage: ./Main <Root URI>"
