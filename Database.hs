module Database where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Monad.Trans (MonadIO, liftIO)
import Text.Email.Validate (EmailAddress)
import qualified Data.Map as Map

data RPSChoice = Rock | Paper | Scissors deriving (Read, Show, Eq)

data RPSGame =
	RPSGameStart (EmailAddress, RPSChoice) |
	RPSGameFinish (EmailAddress, RPSChoice) (EmailAddress, RPSChoice)

data DatabaseMessage =
	SetKey String RPSGame |
	GetKey String (Chan (Maybe RPSGame))

type DatabaseConnection = Chan DatabaseMessage

dbGet :: (MonadIO m) => DatabaseConnection -> String -> m (Maybe RPSGame)
dbGet db k = syncCall db (GetKey k)

dbSet :: (MonadIO m) => DatabaseConnection -> String -> RPSGame -> m ()
dbSet db k v = liftIO $ writeChan db (SetKey k v)

syncCall :: (MonadIO m) => Chan a -> (Chan b -> a) -> m b
syncCall c v = liftIO $ do
	r <- newChan
	writeChan c (v r)
	readChan r

database :: DatabaseConnection -> IO ()
database chan = loop Map.empty
	where
	loop m = do
		msg <- readChan chan
		case msg of
			SetKey k v -> loop (Map.insert k v m)
			GetKey k c -> do
				writeChan c (Map.lookup k m)
				loop m
