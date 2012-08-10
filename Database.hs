module Database where

import Control.Concurrent
import Control.Monad.Trans (liftIO)

import Data.Map (Map)
import qualified Data.Map as Map

database chan = loop Map.empty
	where
	loop m = do
		msg <- readChan chan
		case msg of
			SetKey k v -> loop (Map.insert k v m)
			GetKey k c -> do
				writeChan c (Map.lookup k m)
				loop m

data DatabaseMessage =
	SetKey String String |
	GetKey String (Chan (Maybe String))

syncCall c v = liftIO $ do
	r <- newChan
	writeChan c (v r)
	readChan r

dbGet db k = syncCall db (GetKey k)

dbSet db k v = liftIO $ writeChan db (SetKey k v)
