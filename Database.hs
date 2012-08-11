module Database where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Map as Map

data RPSChoice = Rock | Paper | Scissors deriving (Read, Show, Eq)

data RPSGame =
	RPSGameStart RPSChoice |
	RPSGameFinish RPSChoice RPSChoice

data DatabaseMessage =
	SetKey String RPSGame |
	GetKey String (Chan (Maybe RPSGame))

dbGet :: (MonadIO m) => Chan DatabaseMessage -> String -> m (Maybe RPSGame)
dbGet db k = syncCall db (GetKey k)

dbSet :: (MonadIO m) => Chan DatabaseMessage -> String -> RPSGame -> m ()
dbSet db k v = liftIO $ writeChan db (SetKey k v)

syncCall :: (MonadIO m) => Chan a -> (Chan b -> a) -> m b
syncCall c v = liftIO $ do
	r <- newChan
	writeChan c (v r)
	readChan r

database :: Chan DatabaseMessage -> IO ()
database chan = loop Map.empty
	where
	loop m = do
		msg <- readChan chan
		case msg of
			SetKey k v -> loop (Map.insert k v m)
			GetKey k c -> do
				writeChan c (Map.lookup k m)
				loop m
